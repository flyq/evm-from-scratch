use std::collections::VecDeque;

use ethers_core::types::I256;
use primitive_types::U256;
use sha3::{Digest, Keccak256};

mod valids;

pub struct EvmResult {
    pub stack: Vec<U256>,
    pub success: bool,
}

pub struct Tx {
    from: String,
    to: String,
    origin: String,
    gasprice: String,
    value: String,
    data: String,
}

pub struct Block {
    basefee: String,
    coinbase: String,
    timestamp: String,
    number: String,
    difficulty: String,
    gaslimit: String,
    chainid: String,
}

pub fn evm(_code: impl AsRef<[u8]>) -> EvmResult {
    let mut stack: VecDeque<U256> = VecDeque::new();
    let mut memory: Vec<u8> = vec![0; 32 * 64];
    let mut mem_ptr: usize = 0;
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
            0x30 => {}
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

            // INVALID
            0xfe => {
                success = Some(false);
                break;
            }
            _ => {
                panic!();
            }
        }

        if opcode == 0x00 {
            break;
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
