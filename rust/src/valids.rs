/// Mapping of valid jump destination from code.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Valids(Vec<bool>);

impl Valids {
    /// Create a new valid mapping from given code bytes.
    pub fn new(code: &[u8]) -> Self {
        let mut valids: Vec<bool> = Vec::with_capacity(code.len());
        valids.resize(code.len(), false);

        let mut i = 0;
        while i < code.len() {
            let opcode = code[i];
            if opcode == 0x5b {
                valids[i] = true;
                i += 1;
            } else if (0x60..=0x7f).contains(&opcode) {
                let v = opcode - 0x60 + 1;
                i += v as usize + 1;
            } else {
                i += 1;
            }
        }

        Valids(valids)
    }

    /// Returns `true` if the position is a valid jump destination. If
    /// not, returns `false`.
    pub fn is_valid(&self, position: usize) -> bool {
        if position >= self.0.len() {
            return false;
        }

        if !self.0[position] {
            return false;
        }

        true
    }
}
