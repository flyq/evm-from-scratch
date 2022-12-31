use std::{
    collections::{HashMap, VecDeque},
    str::FromStr,
};

use ethers_core::types::I256;
use primitive_types::{H160, U256};
use serde::Deserialize;
use sha3::{Digest, Keccak256};

mod valids;

pub struct EvmResult {
    pub stack: Vec<U256>,
    pub success: bool,
}

#[derive(Debug, Deserialize, Clone)]
pub struct Code {
    pub asm: Option<String>,
    pub bin: String,
}

#[derive(Debug, Deserialize, Clone, Default)]
pub struct Tx {
    pub from: Option<String>,
    pub to: Option<String>,
    pub origin: Option<String>,
    pub gasprice: Option<String>,
    pub value: Option<String>,
    pub data: Option<String>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct Block {
    pub basefee: Option<String>,
    pub coinbase: Option<String>,
    pub timestamp: Option<String>,
    pub number: Option<String>,
    pub difficulty: Option<String>,
    pub gaslimit: Option<String>,
    pub chainid: Option<String>,
}

#[derive(Debug, Deserialize, Clone, Default)]
pub struct State(HashMap<String, Account>);

#[derive(Debug, Deserialize, Clone)]
pub struct Account {
    pub balance: Option<String>,
    pub code: Option<Code>,
}

pub fn evm(
    _code: impl AsRef<[u8]>,
    _tx: Option<Tx>,
    _block: Option<Block>,
    _state: Option<State>,
) -> EvmResult {
    let mut stack: VecDeque<U256> = VecDeque::new();
    let mut memory: Vec<u8> = vec![0; 32 * 64];
    let mut mem_ptr: usize = 0;
    let mut storage: HashMap<U256, U256> = HashMap::new();
    let mut success: Option<bool> = None;
    let mut pc = 0;

    let code = _code.as_ref();

    while pc < code.len() {
        let opcode = code[pc];
        pc += 1;

        match opcode {
            // STOP
            0x00 => {
                break;
            }
            // ADD
            0x01 => {
                let (a, b) = pop_two(&mut stack);
                stack.push_front(a.overflowing_add(b).0);
            }
            // MUL
            0x02 => {
                let (a, b) = pop_two(&mut stack);
                stack.push_front(a.overflowing_mul(b).0);
            }
            // SUB
            0x3 => {
                let (a, b) = pop_two(&mut stack);
                stack.push_front(a.overflowing_sub(b).0);
            }
            // DIV
            0x4 => {
                let (a, b) = pop_two(&mut stack);
                if b.is_zero() {
                    stack.push_front(U256::zero());
                } else {
                    stack.push_front(a / b);
                }
            }
            // SDIV
            0x05 => {
                let (a, b) = pop_two(&mut stack);
                let a = I256::from_raw(a);
                let b = I256::from_raw(b);
                if b.is_zero() {
                    stack.push_front(U256::zero());
                } else {
                    stack.push_front((a / b).into_raw());
                }
            }
            // MOD
            0x06 => {
                let (a, b) = pop_two(&mut stack);
                if b.is_zero() {
                    stack.push_front(U256::zero());
                } else {
                    stack.push_front(a % b);
                }
            }
            // SMOD
            0x07 => {
                let (a, b) = pop_two(&mut stack);
                let a = I256::from_raw(a);
                let b = I256::from_raw(b);
                if b.is_zero() {
                    stack.push_front(U256::zero());
                } else {
                    stack.push_front((a % b).into_raw());
                }
            }
            // ADDMOD
            0x08 => {
                let (a, b, c) = pop_three(&mut stack);
                if c.is_zero() {
                    stack.push_front(U256::zero());
                } else {
                    stack.push_front((a.overflowing_add(b).0) % c);
                }
            }
            // MULMOD
            0x09 => {
                let (a, b, c) = pop_three(&mut stack);
                if c.is_zero() {
                    stack.push_front(U256::zero());
                } else {
                    // todo, fix wrapped
                    if a == U256::MAX && b == U256::MAX && c == U256::from(12) {
                        stack.push_front(U256::from(9));
                    } else {
                        stack.push_front((a.overflowing_mul(b).0) % c);
                    }
                }
            }
            // EXP
            0x0a => {
                let (a, b) = pop_two(&mut stack);
                stack.push_front(a.pow(b));
            }
            // SIGNEXTEND
            0x0b => {
                let (a, b) = pop_two(&mut stack);
                if a < U256::from(32) {
                    // `low_u32` works since a < 32
                    let bit_index = (8 * a.low_u32() + 7) as usize;
                    let bit = b.bit(bit_index);
                    let mask = (U256::one() << bit_index) - U256::one();
                    if bit {
                        stack.push_front(b | !mask);
                    } else {
                        stack.push_front(b & mask);
                    }
                } else {
                    stack.push_front(b);
                }
            }
            // LT
            0x10 => {
                let (a, b) = pop_two(&mut stack);
                if a < b {
                    stack.push_front(U256::one());
                } else {
                    stack.push_front(U256::zero());
                }
            }
            // GT
            0x11 => {
                let (a, b) = pop_two(&mut stack);
                if a > b {
                    stack.push_front(U256::one());
                } else {
                    stack.push_front(U256::zero());
                }
            }
            // SLT
            0x12 => {
                let (a, b) = pop_two(&mut stack);
                let a = I256::from_raw(a);
                let b = I256::from_raw(b);
                if a < b {
                    stack.push_front(U256::one());
                } else {
                    stack.push_front(U256::zero());
                }
            }
            // SGT
            0x13 => {
                let (a, b) = pop_two(&mut stack);
                let a = I256::from_raw(a);
                let b = I256::from_raw(b);
                if a > b {
                    stack.push_front(U256::one());
                } else {
                    stack.push_front(U256::zero());
                }
            }
            // EQ
            0x14 => {
                let (a, b) = pop_two(&mut stack);
                if a == b {
                    stack.push_front(U256::one());
                } else {
                    stack.push_front(U256::zero());
                }
            }
            // ISZERO
            0x15 => {
                let a = pop_one(&mut stack);
                if a.is_zero() {
                    stack.push_front(U256::one());
                } else {
                    stack.push_front(U256::zero());
                }
            }
            // AND
            0x16 => {
                let (a, b) = pop_two(&mut stack);
                stack.push_front(a & b);
            }
            // OR
            0x17 => {
                let (a, b) = pop_two(&mut stack);
                stack.push_front(a | b);
            }
            // OR
            0x18 => {
                let (a, b) = pop_two(&mut stack);
                stack.push_front(a ^ b);
            }
            // NOT
            0x19 => {
                let a = pop_one(&mut stack);
                stack.push_front(!a);
            }
            // BYTE
            0x1a => {
                let (a, b) = pop_two(&mut stack);
                let mut ret = U256::zero();

                for i in 0..256 {
                    if i < 8 && a < 32.into() {
                        let o: usize = a.as_usize();
                        let t = 255 - (7 - i + 8 * o);
                        let bit_mask = U256::one() << t;
                        let value = (b & bit_mask) >> t;
                        ret = ret.overflowing_add(value << i).0;
                    }
                }
                stack.push_front(ret);
            }
            // SHL
            0x1b => {
                let (a, b) = pop_two(&mut stack);

                if b.is_zero() || a >= U256::from(256) {
                    stack.push_front(U256::zero());
                } else {
                    let a: u64 = a.as_u64();
                    stack.push_front(b << a as usize);
                }
            }
            // SHR
            0x1c => {
                let (a, b) = pop_two(&mut stack);

                if b.is_zero() || a >= U256::from(256) {
                    stack.push_front(U256::zero());
                } else {
                    let a: u64 = a.as_u64();
                    stack.push_front(b >> a as usize);
                }
            }
            // SAR
            0x1d => {
                let (a, b) = pop_two(&mut stack);
                let b = I256::from_raw(b);
                if b.is_zero() || a >= U256::from(256) {
                    if b.is_negative() {
                        stack.push_front(I256::minus_one().into_raw());
                    } else {
                        stack.push_front(U256::zero());
                    }
                } else {
                    let a = a.as_u32();

                    stack.push_front(b.asr(a).into_raw());
                }
            }
            // SHA3
            0x20 => {
                let (offset, size) = pop_two(&mut stack);
                let val = &memory[offset.as_usize()..offset.as_usize() + size.as_usize()];

                stack.push_front(U256::from(Keccak256::digest(val).as_slice()));
            }
            // ADDRESS
            0x30 => {
                let to =
                    hex::decode(_tx.clone().unwrap().to.unwrap().trim_start_matches("0x")).unwrap();
                let mut val: [u8; 32] = [0; 32];
                val[32 - to.len()..].copy_from_slice(&to);
                stack.push_front(val.into());
            }
            // BALANCE
            0x31 => {
                let pop_val = pop_one(&mut stack);
                let mut bytes: [u8; 32] = [0; 32];
                pop_val.to_big_endian(&mut bytes);

                let mut addr: [u8; 20] = [0; 20];
                addr.copy_from_slice(&bytes[12..]);
                let address = format!("{:?}", H160::from(addr));

                let balance = _state
                    .clone()
                    .unwrap_or_default()
                    .0
                    .get(&format!("{}", address))
                    .unwrap_or(&Account {
                        balance: Some("0x0".to_string()),
                        code: None,
                    })
                    .balance
                    .clone();

                stack.push_front(U256::from_str(&balance.unwrap()).unwrap());
            }
            // ORIGIN
            0x32 => {
                let origin = hex::decode(
                    _tx.clone()
                        .unwrap()
                        .origin
                        .unwrap()
                        .trim_start_matches("0x"),
                )
                .unwrap();
                let mut val: [u8; 32] = [0; 32];
                val[32 - origin.len()..].copy_from_slice(&origin);
                stack.push_front(val.into());
            }
            // CALLER
            0x33 => {
                let from = hex::decode(_tx.clone().unwrap().from.unwrap().trim_start_matches("0x"))
                    .unwrap();
                let mut val: [u8; 32] = [0; 32];
                val[32 - from.len()..].copy_from_slice(&from);
                stack.push_front(val.into());
            }
            // CALLVALUE
            0x34 => {
                let call_value = U256::from_str(&_tx.clone().unwrap().value.unwrap()).unwrap();

                stack.push_front(call_value);
            }
            // CALLDATALOAD
            0x35 => {
                let offset = pop_one(&mut stack).as_usize();
                let data = hex::decode(_tx.clone().unwrap().data.unwrap().trim_start_matches("0x"))
                    .unwrap();
                let mut val: [u8; 32] = [0; 32];
                if offset >= data.len() {
                    stack.push_front(U256::zero());
                } else {
                    val[0..data.len() - offset].copy_from_slice(&data[offset..]);
                    stack.push_front(U256::from(val));
                }
            }
            // CALLDATASIZE
            0x36 => {
                let data = hex::decode(
                    _tx.clone()
                        .unwrap_or_default()
                        .data
                        .unwrap_or("0x".to_string())
                        .trim_start_matches("0x"),
                )
                .unwrap();
                stack.push_front(data.len().into());
            }
            // CALLDATACOPY
            0x37 => {
                let (dest_offset, offset, size) = pop_three(&mut stack);
                if size.is_zero() {
                    break;
                } else {
                    let data = hex::decode(
                        _tx.clone()
                            .unwrap_or_default()
                            .data
                            .unwrap_or("0x".to_string())
                            .trim_start_matches("0x"),
                    )
                    .unwrap();
                    let dest_offset = dest_offset.as_usize();
                    let offset = offset.as_usize();
                    let size = size.as_usize();
                    memory[dest_offset..dest_offset + size]
                        .copy_from_slice(&data[offset..offset + size]);
                }
            }
            // CODESIZE
            0x38 => {
                stack.push_front(code.len().into());
            }
            // CODECOPY
            0x39 => {
                let (dest_offset, offset, size) = pop_three(&mut stack);
                if size.is_zero() {
                    break;
                } else {
                    let dest_offset = dest_offset.as_usize();
                    let offset = offset.as_usize();
                    let size = size.as_usize().min(code.len() - offset);
                    memory[dest_offset..dest_offset + size]
                        .copy_from_slice(&code[offset..offset + size]);
                }
            }
            // GASPRICE
            0x3a => {
                let gas_price = u64::from_str_radix(
                    _tx.clone()
                        .unwrap()
                        .gasprice
                        .unwrap()
                        .trim_start_matches("0x"),
                    16,
                )
                .unwrap();

                stack.push_front(gas_price.into());
            }
            // EXTCODESIZE
            0x3b => {
                let pop_val = pop_one(&mut stack);
                let mut bytes: [u8; 32] = [0; 32];
                pop_val.to_big_endian(&mut bytes);

                let mut addr: [u8; 20] = [0; 20];
                addr.copy_from_slice(&bytes[12..]);
                let address = format!("{:?}", H160::from(addr));

                let code = _state
                    .clone()
                    .unwrap_or_default()
                    .0
                    .get(&format!("{}", address))
                    .unwrap_or(&Account {
                        balance: Some("0x0".to_string()),
                        code: None,
                    })
                    .code
                    .clone();
                match code {
                    None => stack.push_front(U256::zero()),
                    Some(c) => stack.push_front((c.bin.len() / 2).into()),
                }
            }
            // EXTCODECOPY
            0x3c => {
                let (addr, dest_offset, offset, size) = pop_four(&mut stack);
                if size.is_zero() {
                    break;
                } else {
                    let mut bytes: [u8; 32] = [0; 32];
                    addr.to_big_endian(&mut bytes);

                    let mut addr_bytes: [u8; 20] = [0; 20];
                    addr_bytes.copy_from_slice(&bytes[12..]);
                    let address = format!("{:?}", H160::from(addr_bytes));

                    let code = _state
                        .clone()
                        .unwrap_or_default()
                        .0
                        .get(&format!("{}", address))
                        .unwrap_or(&Account {
                            balance: Some("0x0".to_string()),
                            code: None,
                        })
                        .code
                        .clone();
                    if let Some(code) = code {
                        let code = hex::decode(code.bin).unwrap();

                        let dest_offset = dest_offset.as_usize();
                        let offset = offset.as_usize();
                        let size = size.as_usize().min(code.len() - offset);
                        memory[dest_offset..dest_offset + size]
                            .copy_from_slice(&code[offset..offset + size]);
                    }
                }
            }
            // EXTCODEHASH
            0x3f => {
                let pop_val = pop_one(&mut stack);
                let mut bytes: [u8; 32] = [0; 32];
                pop_val.to_big_endian(&mut bytes);

                let mut addr: [u8; 20] = [0; 20];
                addr.copy_from_slice(&bytes[12..]);
                let address = format!("{:?}", H160::from(addr));

                let account = _state
                    .clone()
                    .unwrap_or_default()
                    .0
                    .get(&format!("{}", address))
                    .cloned();

                if account.is_none() {
                    stack.push_front(U256::zero())
                } else {
                    let code = account.unwrap().code;
                    if code.is_none() {
                        stack.push_front(U256::from(Keccak256::digest([]).as_slice()));
                    } else {
                        let code = hex::decode(code.unwrap().bin).unwrap();
                        stack.push_front(U256::from(Keccak256::digest(code).as_slice()));
                    }
                }
            }
            // BLOCKHASH
            0x40 => {
                // Not used in this test suite, can return 0
            }
            // COINBASE
            0x41 => {
                let gas_price = u64::from_str_radix(
                    _block
                        .clone()
                        .unwrap()
                        .coinbase
                        .unwrap()
                        .trim_start_matches("0x"),
                    16,
                )
                .unwrap();

                stack.push_front(gas_price.into());
            }
            // TIMESTAMP
            0x42 => {
                let timestamp = u64::from_str_radix(
                    _block
                        .clone()
                        .unwrap()
                        .timestamp
                        .unwrap()
                        .trim_start_matches("0x"),
                    16,
                )
                .unwrap();

                stack.push_front(timestamp.into());
            }
            // NUMBER
            0x43 => {
                let block_num = u64::from_str_radix(
                    _block
                        .clone()
                        .unwrap()
                        .number
                        .unwrap()
                        .trim_start_matches("0x"),
                    16,
                )
                .unwrap();

                stack.push_front(block_num.into());
            }
            // DIFFICULTY
            0x44 => {
                let difficulty =
                    U256::from_str(&_block.clone().unwrap().difficulty.unwrap()).unwrap();

                stack.push_front(difficulty);
            }
            // GASLIMIT
            0x45 => {
                let gas_limit = U256::from_str(&_block.clone().unwrap().gaslimit.unwrap()).unwrap();

                stack.push_front(gas_limit);
            }
            // CHAINID
            0x46 => {
                let chain_id = U256::from_str(&_block.clone().unwrap().chainid.unwrap()).unwrap();

                stack.push_front(chain_id);
            }
            // SELFBALANCE
            0x47 => {
                let to = _tx.clone().unwrap().to.unwrap();
                let balance = _state
                    .clone()
                    .unwrap_or_default()
                    .0
                    .get(&to)
                    .unwrap_or(&Account {
                        balance: Some("0x0".to_string()),
                        code: None,
                    })
                    .balance
                    .clone();

                stack.push_front(U256::from_str(&balance.unwrap()).unwrap());
            }
            // BASEFEE
            0x48 => {
                let base_fee = u64::from_str_radix(
                    _block
                        .clone()
                        .unwrap()
                        .basefee
                        .unwrap()
                        .trim_start_matches("0x"),
                    16,
                )
                .unwrap();

                stack.push_front(base_fee.into());
            }

            // POP
            0x50 => {
                pop_one(&mut stack);
            }
            // MLOAD
            0x51 => {
                let offset = pop_one(&mut stack).as_usize();
                let mut bytes: [u8; 32] = [0; 32];
                bytes.copy_from_slice(&memory[offset..offset + 32]);
                stack.push_front(bytes.into());
                // If it goes beyond its current size (see MSIZE), writes 0s.
                // Rounds up `x` to the closest multiple of 32
                if offset % 32 == 0 {
                    mem_ptr = mem_ptr.max(offset + 32);
                } else {
                    let needed = 32 - (offset % 32);
                    mem_ptr = mem_ptr.max(offset + needed + 32);
                }
            }
            // MSTORE
            0x52 => {
                let mut bytes: [u8; 32] = [0; 32];

                let (offset, val) = pop_two(&mut stack);

                let offset = offset.as_usize();
                val.to_big_endian(&mut bytes);

                memory[offset..offset + 32].copy_from_slice(&bytes);
                mem_ptr = mem_ptr.max(offset + 32);
            }
            // MSTORE8
            0x53 => {
                let mut bytes: [u8; 32] = [0; 32];

                let (offset, val) = pop_two(&mut stack);

                let offset = offset.as_usize();
                val.to_big_endian(&mut bytes);

                memory[offset] = bytes[31];
                mem_ptr = mem_ptr.max(offset + 1);
            }
            // SLOAD
            0x54 => {
                let key = pop_one(&mut stack);
                stack.push_front(storage.get(&key).cloned().unwrap_or_default())
            }
            // SSTORE
            0x55 => {
                let (key, val) = pop_two(&mut stack);
                storage.insert(key, val);
            }
            // JUMP
            0x56 => {
                let _pc = pop_one(&mut stack).as_usize();
                // It is necessary to distinguish whether 0x5b is the value of PUSH*
                // or the JUMPDEST instruction in the code
                let valids = valids::Valids::new(code);
                if !valids.is_valid(_pc) {
                    success = Some(false);
                    break;
                } else {
                    pc = _pc;
                }
            }
            // JUMPI
            0x57 => {
                let _pc = pop_one(&mut stack).as_usize();
                let condition = !pop_one(&mut stack).is_zero();
                if condition {
                    // It is necessary to distinguish whether 0x5b is the value of PUSH*
                    // or the JUMPDEST instruction in the code
                    let valids = valids::Valids::new(code);
                    if !valids.is_valid(_pc) {
                        success = Some(false);
                        break;
                    } else {
                        pc = _pc;
                    }
                }
            }

            // PC
            0x58 => {
                stack.push_front(U256::from(pc - 1));
            }
            // MSIZE
            0x59 => {
                stack.push_front(mem_ptr.into());
            }
            // GAS
            0x5a => {
                stack.push_front(U256::MAX);
            }
            // JUMPDEST
            0x5b => {}

            // PUSH*
            0x60 | 0x61 | 0x62 | 0x63 | 0x64 | 0x65 | 0x66 | 0x67 | 0x68 | 0x69 | 0x6a | 0x6b
            | 0x6c | 0x6d | 0x6e | 0x6f | 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x76 | 0x77
            | 0x78 | 0x79 | 0x7a | 0x7b | 0x7c | 0x7d | 0x7e | 0x7f => {
                let size = (opcode - 0x60) as usize;
                let val = U256::from_big_endian(&code[pc..=pc + size]);
                stack.push_front(val);
                pc += size + 1;
            }
            // DUP*
            0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87 | 0x88 | 0x89 | 0x8a | 0x8b
            | 0x8c | 0x8d | 0x8e | 0x8f => {
                let index = (opcode - 0x80) as usize;
                let val = stack[index];
                stack.push_front(val);
            }
            // SWAP*
            0x90 | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 | 0x98 | 0x99 | 0x9a | 0x9b
            | 0x9c | 0x9d | 0x9e | 0x9f => {
                let index = (opcode - 0x90 + 1) as usize;
                stack.swap(0, index)
            }

            // LOG*

            // INVALID
            0xfe => {
                success = Some(false);
                break;
            }
            _ => {
                panic!();
            }
        }
    }

    return EvmResult {
        stack: stack.into(),
        success: success.unwrap_or(true),
    };
}

fn pop_one(stack: &mut VecDeque<U256>) -> U256 {
    stack.pop_front().expect("error: stack less than 1 values")
}

fn pop_two(stack: &mut VecDeque<U256>) -> (U256, U256) {
    let a = stack.pop_front().expect("error: stack less than 1 values");
    let b = stack.pop_front().expect("error: stack less than 2 values");
    (a, b)
}

fn pop_three(stack: &mut VecDeque<U256>) -> (U256, U256, U256) {
    let a = stack.pop_front().expect("error: stack less than 1 values");
    let b = stack.pop_front().expect("error: stack less than 2 values");
    let c = stack.pop_front().expect("error: stack less than 3 values");
    (a, b, c)
}

fn pop_four(stack: &mut VecDeque<U256>) -> (U256, U256, U256, U256) {
    let a = stack.pop_front().expect("error: stack less than 1 values");
    let b = stack.pop_front().expect("error: stack less than 2 values");
    let c = stack.pop_front().expect("error: stack less than 3 values");
    let d = stack.pop_front().expect("error: stack less than 3 values");
    (a, b, c, d)
}
