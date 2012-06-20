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

;=== settings

target: 'reds	;  Red/System . More targets will be added later

;=== support

to-dtype: func [
	type
][
	to word! join type "!"
]

to-set-dtype: func [
	type
][
	to set-word! join type "!"
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

convert-dtype: func[
	type
][
	type
]

to-filename: func [
	name
][
	; convert "file" and <file> to %file
	replace/all name "<" ""
	replace/all name ">" ""
	replace/all name {"} ""
	to file! name
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


comment-rule: ["/*" thru "*/"]

define-rule: [
	"#DEFINE" (debug #define-rule)
	spaces
	copy const-name to-space
	any-spaces
	copy const-value to-space
	(emit 'define)
]

enum-rule: [
	"enum" (debug #ENUM)
	spaces
	copy enum-name some chars
	spaces
	"{"
	(init 'enum)
	some [
		spaces
		copy var some chars
		opt [any-spaces  "=" any-spaces copy value some chars (value: hex-to-int value)]	; FIXME: not all values are hex number!
		["," | newline]	; last var is not followed by comma
		(emit 'enum-values) ; FIXME: not really emit, but support function
	]
	any-spaces "}"
	any-spaces ";"
	(emit 'enum)
]

extern-rule: [
	"extern" (debug #EXTERN)
	spaces
	copy val-type to-space
	spaces
	copy val-name to ";"
	(emit 'extern)
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
		init 'func
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
		(emit 'func-values)
	]
	thru ";"
	(emit 'func)
]

if-defined-rule: [
	"#ifdef" (debug #IFDEF)
	thru ; FIXME
	"#endif" (debug #IFDEF_END)
]

if-not-defined-rule: [
	"#ifndef" (debug #IFNDEF)
	spaces
	copy val-name to-space
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
	(emit 'include)
]

struct-rule: [
	"struct" spaces copy struct-name to-space thru "{" (init 'struct)
	struct-inner-rule
]

struct-inner-rule: [
	some [
		any-spaces
		copy val-type some chars
		spaces
		copy val-name some [chars | "*"] ; can be pointer
		any-spaces
		thru ";"
		(emit 'struct)
	]
]

typedef-rule: [
	; TODO: struct-type? is hardcoded, make it more abstract, so there's no need for struct-type?
	"typedef" (
		debug #TYPEDEF
		struct-type?: false
	)
	spaces
	[
		"struct" (struct-type?: true init 'struct)
		thru "{"
		struct-inner-rule
		thru "}"
	|	copy def-type to-space spaces
	]
	any-spaces
	copy def-name some chars
	any-spaces
	thru ";"
	(emit 'typedef)
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

;=== emitters & initters

emitters: [
	reds [
		define [
			repend reds-code [#define to word! const-name const-value]
			new-line skip tail reds-code -3 true
			debug #define-rule_END
		]
		enum [
			debug #ENUM_END
			enum-reds: copy []
			foreach [name value] enum-values [
				repend enum-reds [to-set-dtype name value]
				new-line skip tail enum-reds -2 true
			]
			repend reds-code [#enum to-set-dtype enum-name enum-reds]
		]
		enum-values [
			append var-names var
			repend var-names-rule ['| var]
			repend enum-values [var value]
			value: value + 1	; update value from auto enum
		]
		extern [
			; TODO: what to emit?
		]
		func [
			repend import-funcs [to set-word! func-name func-name func-data]
			new-line skip tail import-funcs -3 true
		]
		func-values [
			repend func-data [to word! param-name reduce [to-dtype param-type]]
			new-line skip tail func-data -2 true
		]
		include [
			include-name: to-filename include-name
			append includes include-name		
			a: ask rejoin ["Include file " include-name "? (Yes/no) "] ; TODO: move to separate function
			either equal? #"y" first a [
				print ["^/^/Including file... " include-name]
				parse/all read include-name main-rule
				print ["^/^/processed..." include-name "^/^/"]
			][
	;			include-name: old-include-name
			]
		]
		struct [
			repend struct-values [to word! val-name reduce [to-dtype val-type]]
			new-line skip tail struct-values -2 true
		]
		typedef [
			repend var-names-rule ['| def-name]
			debug [#TYPEDEF_END def-name]
			if struct-type? [
				repend reds-code [to-set-dtype def-name 'alias struct! struct-values]
				new-line skip tail reds-code -4 true
			]
		]
	]
]

initters: [
	reds [
		enum [
			enum-values: copy []
			value: 0
		]
		func [
		;	import-funcs: copy []
			func-data: copy []
		]
		struct [
			struct-values: copy []
		]
	]
]

emit: func [
	fn "Function name" ; FIXME> not a function, needs better name
][
	do emitters/:target/:fn
]

init: func [
	fn "Function name" ; FIXME> not a function, needs better name
][
	do initters/:target/:fn
]

; ==== test

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
