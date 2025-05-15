use super::Opcode;

/// Creates instruction bytes from an opcode and operands.
pub fn make(opcode: Opcode, operands: &[usize]) -> Vec<u8> {
    let op_widths = opcode.operand_widths();
    let len = op_widths.iter().map(|&b| b as usize).sum::<usize>() + 1;

    let mut instruction = vec![0; len];
    instruction[0] = opcode as u8;

    let mut offset = 1;
    for (o, w) in operands.iter().zip(op_widths) {
        match w {
            2 => {
                instruction[offset] = (o >> 8) as u8;
                instruction[offset + 1] = *o as u8;
            }
            1 => instruction[offset] = *o as u8,
            _ => {}
        }
        offset += w as usize;
    }

    instruction
}

/// Reads operands from instruction bytes.
pub fn read_operands(opcode: &Opcode, ins: &[u8]) -> (Vec<usize>, usize) {
    let widths = opcode.operand_widths();

    let mut operands = vec![];
    let mut offset = 0;

    for w in widths {
        match w {
            2 => {
                operands.push(read_u16(&ins[offset..offset + 2]) as usize);
                offset += 2;
            }
            1 => {
                operands.push(ins[offset] as usize);
                offset += 1;
            }
            _ => {}
        }
    }

    (operands, offset)
}

pub fn read_u16(ins: &[u8]) -> u16 {
    u16::from_be_bytes(ins[0..2].try_into().unwrap())
}
