import * as xjs from 'extrajs';
import * as utf8 from 'utf8';

import type {CodeUnit} from '../types';
import {SolidObject} from './SolidObject';
import {SolidBoolean} from './SolidBoolean';



export class SolidString extends SolidObject {
	private readonly codeunits: readonly CodeUnit[];
	constructor (data: string | readonly CodeUnit[]) {
		super();
		this.codeunits = (typeof data === 'string')
			? [...utf8.encode(data)].map((ch) => ch.codePointAt(0)!)
			: data
	}

	/** @override SolidObject */
	get isEmpty(): SolidBoolean {
		return SolidBoolean.fromBoolean(this.codeunits.length === 0);
	}
	/** @override SolidObject */
	protected identical_helper(value: SolidObject): boolean {
		return value instanceof SolidString && xjs.Array.is(this.codeunits, value.codeunits);
	}
}
