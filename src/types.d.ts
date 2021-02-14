export type NonemptyArray<T> = readonly [T, ...readonly T[]]


export type EBNFObject = {
	readonly name: string,
	readonly defn: EBNFChoice,
}

export type EBNFChoice = NonemptyArray<EBNFSequence>

export type EBNFSequence = NonemptyArray<EBNFItem>

export type EBNFItem =
	| string
	| { readonly term: string }
	| { readonly prod: string }
