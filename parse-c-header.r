REBOL[
	Title: "Parse C header (*.h) and emit Red/Source output."
	Author: "Boleslav Brezovsky"
	Copyright: "(c) Boleslav Brezovsky 2012
	License: "BSD"
	Date: "18-6-2012"
	Version: 0.0.1
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

;=== rules

whitespace: charset " ^-"
skip-whitespace: [some [" " | "^-"]]
to-whitespace: [to #" " | to #"^-" | to #"^/"]
chars: charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

spacer: charset reduce [tab newline #" "]
spaces: [some spacer]
any-spaces: [any spacer]
non-space: complement spacer
to-space: [some non-space | end]

;===


enum-rule: [
	thru "enum"
	thru "{"
	(enum-values: copy [])
	some [
		spaces
		copy var to-space spaces "=" (print ["var:" var])
		spaces
		copy value [to "," | to newline] (print ["val:" value])
		thru newline
		(repend enum-values [trim var hex-to-int trim value])
	]
	thru "}"
]

struct-rule: [
	thru "struct" spaces copy struct-name to-space thru "{" (
		struct-values: copy []
	)	
	some [
		spaces
		copy val-type to-space
		spaces
		copy val-name to-space
		any-spaces
		opt [copy struct-name to-space]
		any-spaces
		(repend struct-values [val-type val-name])
		thru ";"
	]
]

comment-rule: [
	"/*" thru "*/"
]

define-rule: [
	thru #DEFINE
	spaces
	copy val-name to-space
	any-spaces
	opt [copy var-name to-space]
]

if-not-defined-rule: [
	thru #IFNDEV
	spaces
	copy val-name
	spaces
	define-rule
]

include-rule: [
	thru #INCLUDE
	spaces
	copy include-name to-space
]

extern-c-begin-rule: [
	thru "EXTERN_C_BEGIN"
]

extern-c-end-rule: [
	thru "EXTERN_C_END"
]

extern-rule: [
	thru "extern"
	spaces
	copy extern-type to-space
	spaces
	copy exern-value to ";" skip
]

typedef-rule: [
	"typedef"
	spaces
	struct-rule
]

function-rule: [
	["void" | "UInt32" | "UInt64" | "int" ] ; FIXME: add more types
	; TODO: add user defined types
	spaces
	copy var-name to "(" skip
	copy var-data to ")" skip
	thru ";"
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

parse test-header [
	some [
		comment-rule
	|	struct-rule
	|	enum-rule
	]
]



halt
