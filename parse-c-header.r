REBOL[
	Title: "Parse C header (*.h) and emit Red/Source output."
	Author: "Boleslav Brezovsky"
	Copyright: "(c) Boleslav Brezovsky 2012"
	License: "BSD"
	Date: "18-6-2012"
	Version: 0.0.1
	To-do: [
		"Datatype conversion"
	]
]


; ==== support

to-dtype: func [
	type
][
	to word! join type "!"
]

hex-conv: func [
	"Convert 0x0001 to 0001h"
	number
][
	either equal? copy/part number 2 "0x" [
		rejoin [skip number 2 "h"]
	][none]
]

hex-to-int: func [
	number
][
	to integer! to issue! skip number 2
]

debug?: true
debug: func[text][
	if debug? [print text]
]

;=== state machine

var-names: copy []
enum-values: copy []
struct-values: copy []

include-name:
old-include-name: none
includes: copy []

reds-code: copy []
import-funcs: copy []
;=== rules

chars: charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

spacer: charset reduce [tab newline #" "]
spaces: [some spacer]
any-spaces: [any spacer]
non-space: complement spacer
to-space: [some non-space | end]

var-names-rule: ["dummy"]


comment-rule: [
	"/*" (debug #comment-rule)
	thru "*/"
	(debug #comment-rule_END)
]

define-rule: [
	"#DEFINE" (debug #define-rule)
	spaces
	copy const-name to-space
	any-spaces
	copy const-value to-space
	(
		; emit define
		repend reds-code [#define to word! const-name const-value]
		new-line skip tail reds-code -3 true
		
		debug #define-rule_END
	)
]

enum-rule: [
	"enum" (debug #ENUM)
	spaces
	copy enum-name some chars
	spaces
	"{"
	(
		enum-values: copy []
		value: 0
	)
	some [
		spaces
		copy var some chars
		opt [any-spaces  "=" any-spaces copy value some chars (value: hex-to-int value)]	; FIXME: not all values are hex number!
		["," | newline]	; last var is not followed by comma
		(
			append var-names var
			repend var-names-rule ['| var]
			repend enum-values [var value]
			value: value + 1	; update value from auto enum
		)
	]
	any-spaces "}"
	any-spaces ";"
	(
		debug #ENUM_END
		print [mold enum-values]
		enum-reds: copy []
		foreach [name value] enum-values [
			repend enum-reds [to set-word! to-dtype name value]
			new-line skip tail enum-reds -2 true
		]
		repend reds-code [
			#enum to set-word! to-dtype enum-name enum-reds
		]
	)
]

extern-rule: [
	"extern" (debug #EXTERN)
	spaces
	copy val-type to-space
	spaces
	copy val-name to ";"
	(
		; emit extern
		; TODO: what to emit?
	)
]

extern-c-begin-rule: [
	"EXTERN_C_BEGIN" (debug #EXTERN_C)
]

extern-c-end-rule: [
	"EXTERN_C_END" (debug #EXTERN_C_END)
]

extern-rule: [
	"extern"
	spaces
	copy extern-type to-space
	spaces
	copy exern-value to ";" skip
]


function-rule: [
;	; FIXME: add more types
;	copy func-type ["void" | "UInt32" | "UInt64" | "int" | var-names-rule] (debug #FUNCT print func-type)

	; this version works with unknown types too (be careful, it will parse everything!)
	copy func-type some chars (
		debug #FUNCT 
		func-data: copy []
	)
	spaces
	copy func-name some chars 
	"("
	some [
		any-spaces
		opt "const"
		copy param-name some chars
		spaces
		copy param-type some ["*" | chars]
		any-spaces
		["," | ")"] ; FIXME: theoretically may skip past end?
		(
			repend func-data [
				to word! param-name reduce [to-dtype param-type]
			]
			new-line skip tail func-data -2 true
		)
	]
	thru ";"
	(

;	foo: "foo" [
;            fun     [function! [a [integer!] b [integer!] return: [logic!]]]
;            return: [integer!]
;        ]

;	]
;	ClonePixelWand: "ClonePixelWand" [
;		;== Makes an exact copy of the specified wand
;		;-- PixelWand *ClonePixelWand(const PixelWand *wand)
;		wand	[PixelWand!] ;the magick wand.
;		return: [PixelWand!]
;	]
		func-params: copy []

		repend import-funcs [
			to set-word! func-name func-name func-data
		]
		new-line skip tail import-funcs -3 true
	)
]

if-defined-rule: [
	"#ifdef" (debug #IFDEF)
	thru ; FIXME
	"#endif" (debug #IFDEF_END)
]

if-not-defined-rule: [
	"#ifndef" (debug #IFNDEF)
	spaces
	copy val-name to-space (debug ["val: " val-name])
	spaces
	; TODO: redefine define rule to catch both this and self?
	;define-rule
	"#define"
	spaces
	copy val-name to-space
	(debug #IFNDEF_END)
	; TODO: add endif, create inner rule
]

include-rule: [
	"#include" (debug #INCLUDE)
	(old-include-name: include-name)
	spaces
	copy include-name to-space
	(
		; TODO: move to separate function
		replace/all include-name "<" ""
		replace/all include-name ">" ""
		replace/all include-name {"} ""
		include-name: to file! include-name

		append includes include-name		
		a: ask rejoin ["Include file " include-name "? (Yes/no) "]
		either equal? #"y" first a [
			print ["^/^/Including file... " include-name]
			parse/all read include-name main-rule
			print ["^/^/processed..." include-name "^/^/"]
		][
;			include-name: old-include-name
		]
	)
]

struct-rule: [
	"struct" spaces copy struct-name to-space thru "{" (
		struct-values: copy []
	)	
	struct-inner-rule
]

struct-inner-rule: [
	(struct-values: copy [])
	some [
		any-spaces
		copy val-type some chars
		spaces
		copy val-name some [chars | "*"] ; can be pointer
		any-spaces
		thru ";"
		(
			repend struct-values [to word! val-name reduce [to-dtype val-type]]
			new-line skip tail struct-values -2 true
		)
	]
]

typedef-rule: [
	"typedef" (
		debug #TYPEDEF
		struct-type?: false
	)
	spaces
	[
		"struct" (struct-type?: true)
		thru "{"
		struct-inner-rule
		thru "}"
	|	copy def-type to-space spaces
	]
	any-spaces
	copy def-name some chars
	any-spaces
	thru ";"
	(
		repend var-names-rule ['| def-name]
		debug [#TYPEDEF_END def-name]
		if struct-type? [
			repend reds-code [
				to set-word! to-dtype def-name 'alias struct! struct-values
			]
			new-line skip tail reds-code -4 true
		]
	)
]

; -


main-rule: [
	some [
		if-defined-rule
	|	if-not-defined-rule
	|	include-rule
	|	extern-rule
	|	extern-c-begin-rule
	|	define-rule
	|	typedef-rule
;	|	struct-rule
	|	enum-rule
	|	comment-rule
	|	function-rule	; NOTE: must be in the end as current version tries to catch unknown types (see note in function-rule)
	|	spaces p: (print ["SPACES::" replace/all copy/part p 40 "^/" "<enter>" "::"])
	]
]


; === conversion

enumblock-to-reds: func [
	'name
	block
][
	source: reduce [#enum name]
	append/only source collect [
		foreach [var val] block [keep reduce[to set-word! var val]]
	]
	b: source/3
	forskip b 2 [new-line b true]
	b: head b
	source
]


struct-to-reds: func [
	struct-name
	struct-values
	/local struct values
][
	struct: reduce [
		to set-word! to-dtype struct-name 'alias 'struct! collect [
			foreach [type name] struct-values [
				keep reduce [to word! name reduce [to-dtype type]]
			]
		]
	]
	values: last struct
	forskip values 2 [new-line values true]
	values: head values
	struct
]

; ==== test

example-struct: {struct SF_INFO
{	sf_count_t	frames ;		/* Used to be called samples.  Changed to avoid confusion. */
	int			samplerate ;
	int			channels ;
	int			format ;
	int			sections ;
	int			seekable ;
} ;
}

example-enum: {enum
{	SF_STR_TITLE					= 0x01,
	SF_STR_COPYRIGHT				= 0x02,
	SF_STR_SOFTWARE					= 0x03,
	SF_STR_ARTIST					= 0x04,
	SF_STR_COMMENT					= 0x05,
	SF_STR_DATE						= 0x06,
	SF_STR_ALBUM					= 0x07,
	SF_STR_LICENSE					= 0x08,
	SF_STR_TRACKNUMBER				= 0x09,
	SF_STR_GENRE					= 0x10
} ;
}

example-func: {
typedef int SRes;

SRes SzFolder_Decode(const CSzFolder *folder, const UInt64 *packSizes,
    ILookInStream *stream, UInt64 startPos,
    Byte *outBuffer, size_t outSize, ISzAlloc *allocMain);
}

test-func: [
	probe parse/all example-func [
		any-spaces
		typedef-rule
		spaces
		function-rule
		to end
	]
]

test-struct: [
print "parse struct:"
parse/all example-struct struct-rule
print ["name: " struct-name newline mold struct-values]
print mold struct-to-reds struct-name struct-values
print newline
]

test-enum: [
print "parse enum:"
enum-values: copy []
parse/all example-enum enum-rule
print mold enum-values
data: probe enumblock-to-reds test! enum-values
]

test-file: %7z.h
test-header: read test-file

parse/all test-header main-rule
repend reds-code [
	; FIXME: name and decl type hardcoded for testing
	#import reduce [
		"library.dll" 'cdecl import-funcs
	]
]
save %test.reds reds-code


halt
