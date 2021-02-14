import {
	SolidTypeConstant,
	Int16,
	Float64,
} from '../src/validator/'
import {
	InstructionConst,
} from '../src/builder/'



export function typeConstInt(x: bigint): SolidTypeConstant {
	return new SolidTypeConstant(new Int16(x))
}

export function typeConstFloat(x: number): SolidTypeConstant {
	return new SolidTypeConstant(new Float64(x))
}

export function instructionConstInt(x: bigint): InstructionConst {
	return new InstructionConst(new Int16(x))
}
export function instructionConstFloat(x: number): InstructionConst {
	return new InstructionConst(new Float64(x))
}
