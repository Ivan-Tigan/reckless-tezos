(window.webpackJsonp=window.webpackJsonp||[]).push([[254],{328:function(e,a,t){"use strict";t.r(a),t.d(a,"frontMatter",(function(){return l})),t.d(a,"metadata",(function(){return c})),t.d(a,"toc",(function(){return i})),t.d(a,"default",(function(){return b}));var n=t(3),r=t(7),o=(t(0),t(457)),s=t(459),p=t(464),l={id:"map-reference",title:"Map",description:"Map operations",hide_table_of_contents:!0},c={unversionedId:"reference/map-reference",id:"version-0.25.0/reference/map-reference",isDocsHomePage:!1,title:"Map",description:"Map operations",source:"@site/versioned_docs/version-0.25.0/reference/map.md",slug:"/reference/map-reference",permalink:"/docs/reference/map-reference",version:"0.25.0",sidebar:"version-0.25.0/docs",previous:{title:"List",permalink:"/docs/reference/list-reference"},next:{title:"Set",permalink:"/docs/reference/set-reference"}},i=[],m={toc:i};function b(e){var a=e.components,t=Object(r.a)(e,["components"]);return Object(o.b)("wrapper",Object(n.a)({},m,t,{components:a,mdxType:"MDXLayout"}),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function empty : map ('key, 'value)"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val empty : ('key, 'value) map"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let empty: map('key, 'value)"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let empty: map<'key, 'value>"),Object(o.b)("p",null,"Create an empty map."),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),"type move is int * int\ntype register is map (address, move)\n\nconst empty : register = Map.empty\n")),Object(o.b)("p",null,"Or"),Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),"const empty : register = map []\n"))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),"type move = int * int\ntype register = (address, move) map\n\nlet empty : register = Map.empty\n"))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),"type move = (int, int);\ntype register = map (address, move);\n\nlet empty : register = Map.empty\n"))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),"type move = [int, int];\ntype register = map<address, move>;\n\nlet empty: register = Map.empty;\n"))),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function literal : list ('key * 'value) -> map ('key, 'value)"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val literal : ('key * 'value) list -> ('key, 'value) map"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let literal: list(('key, 'value)) => map('key, 'value)"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let literal: (input: list<['key, 'value]>) => map<'key, 'value>"),Object(o.b)("p",null,"Create a non-empty map."),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'const moves : register =\n  Map.literal (list [\n    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));\n    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]);\n')),Object(o.b)("p",null,"Alternative way of creating an empty map:"),Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'const moves_alternative : register =\n  map [\n    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);\n    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)];\n'))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),'let moves : register =\n  Map.literal [\n    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));\n    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]\n'))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),'let moves : register =\n  Map.literal ([\n    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),\n    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);\n'))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),'let moves: register =\n  Map.literal(list([\n    [("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address), [1, 2]],\n    [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), [0, 3]]]));\n'))),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function find_opt : 'key -> map ('key, 'value) -> option 'value"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val find_opt : 'key -> ('key, 'value) map -> 'value option"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let find_opt : ('key, map ('key, 'value)) => option('value)"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let find_opt : (key: 'key, map: map <'key, 'value>) => option <'value>"),Object(o.b)("p",null,"Retrieve a (option) value from a map with the given key. Returns ",Object(o.b)("inlineCode",{parentName:"p"},"None")," if the\nkey is missing and the value otherwise."),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'const my_balance : option (move) =\n  Map.find_opt (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), moves)\n')),Object(o.b)("p",null,"Alternatively:"),Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'const my_balance_alternative : option (move) =\n  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)];\n'))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),'let my_balance : move option =\n  Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves\n'))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),'let my_balance : option (move) =\n  Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, moves);\n'))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),'let my_balance: option<move> =\n  Map.find_opt(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), moves);\n'))),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function update : 'key -> option 'value -> map ('key, 'value) -> map ('key, 'value)"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val update: 'key -> 'value option -> ('key, 'value) map -> ('key, 'value) map"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let update: ('key, option('value), map('key, 'value)) => map('key, 'value)"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let update: (key: 'key, new_value: option<'value>, map: map<'key, 'value>) => map <'key, 'value>"),Object(o.b)("p",null,"Note: when ",Object(o.b)("inlineCode",{parentName:"p"},"None")," is used as a value, the key and associated value is removed\nfrom the map."),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'  const updated_map : register = Map.update(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some (4,9), moves);\n')),Object(o.b)("p",null,"Alternatively:"),Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'\nfunction update (var m : register) : register is\n  block {\n    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9);\n  } with m\n  \n')),Object(o.b)("p",null,"If multiple bindings need to be updated, PascaLIGO offers a ",Object(o.b)("em",{parentName:"p"},"patch\ninstruction")," for maps, similar to that for records."),Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'function assignments (var m : register) : register is\n  block {\n    patch m with map [\n      ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (4,9);\n      ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2)\n    ]\n  } with m\n'))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),'let updated_map : register =\n  Map.update\n    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves\n'))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),'let updated_map : register =\n  Map.update\n    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some ((4,9)), moves);\n'))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),'let updated_map : register =\n  Map.update\n    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), Some ([4, 9]), moves);\n'))),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function get_and_update : key -> option(value) -> map (key, value) -> option(value) * map (key, value)"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val get_and_update : 'key -> 'value option -> ('key, 'value) map -> 'value option * ('key, 'value) map"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let get_and_update : ('key, option('value), map('key, 'value)) => (option('value), map ('key, 'value))"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let get_and_update : (key : 'key, value : option<'value>, map : map<'key, 'value>) => [option<'value>, map<'key, 'value>]"),Object(o.b)("p",null,"Similar to ",Object(o.b)("inlineCode",{parentName:"p"},"update")," but it also returns the value that was previously stored in the map"),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'const updated : option(move) * register = \n  Map.get_and_update (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (Some (4, 9)), moves);\n'))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),'let (old_move_opt, updated_map) : (move option * register) =\n  Map.get_and_update ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4, 9)) moves\n'))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),'let (old_move_opt, updated_map) : (option(move), register) = \n  Map.get_and_update (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (Some (4, 9)), moves);\n'))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),'let [old_move, updated_map] : [option<move>, register] = \n  Map.get_and_update (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), (Some([24, 48] as move)), moves);\n'))),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function add : 'key -> 'value -> map ('key, 'value) -> map ('key, 'value)"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val add : 'key -> 'value -> ('key, 'value) map  -> ('key, 'value) map"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let add: ('key, 'value, map('key, 'value)) => map('key, 'value)"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let add: (key: 'key, value: 'value, map: map<'key, 'value>) => map<'key, 'value>"),Object(o.b)("p",null,"Returns a new map with key-value pair added to the input map"),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'const added_item : register = Map.add (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4, 9), moves)\n'))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),'let add (m : register) : register =\n  Map.add\n    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) m\n'))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),'let add = (m: register): register =>\n  Map.add\n    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4,9), m);\n'))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),'let add = (m: register): register =>\n  Map.add\n    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), [4, 9], m);\n'))),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function remove : 'key -> map ('key, 'value) -> map ('key, 'value)"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val remove : 'key -> ('key, 'value) map -> ('key, 'value) map"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let remove: ('key, map('key, 'value)) => map('key, 'value)"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let remove: ('key: key, map: map<'key, 'value>) => map<'key, 'value>"),Object(o.b)("p",null,"Returns a new map with key-value pair removed from the input map"),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'  const updated_map : register = \n    Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)\n')),Object(o.b)("p",null,"Alternatively, the instruction ",Object(o.b)("inlineCode",{parentName:"p"},"remove key from map m")," removes the key\n",Object(o.b)("inlineCode",{parentName:"p"},"key")," from the map ",Object(o.b)("inlineCode",{parentName:"p"},"m"),"."),Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'function rem (var m : register) : register is\n  block {\n    remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) from map moves\n  } with m\n\nconst updated_map : register = rem (moves)\n'))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),'let updated_map : register =\n  Map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves\n'))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),'let updated_map : register =\n  Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)\n'))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),'let updated_map : register =\n  Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), moves);\n'))),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function iter : ((key, value) -> unit) -> map (key, value) -> unit"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val iter : (('key * 'value) -> unit) -> ('key, 'value) map -> unit"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let iter: ((('key, 'value)) => unit, map('key, 'value)) => unit"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let iter: (iter: (['key, 'value]) => unit, map: map<'key, 'value>) => unit"),Object(o.b)("p",null,"Iterate over key-value pairs in a map"),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'function iter_op (const m : register) : unit is\n  block {\n    function iterated (const i : address; const j : move) : unit is\n      if j.1 > 3 then Unit else (failwith ("Below range.") : unit)\n  } with Map.iter (iterated, m)\n'))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),"let iter_op (m : register) : unit =\n  let predicate = fun (i,j : address * move) -> assert (j.0 > 3)\n  in Map.iter predicate m\n"))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),"let iter_op = (m : register) : unit => {\n  let predicate = ((i,j) : (address, move)) => assert (j[0] > 3);\n  Map.iter (predicate, m);\n};\n"))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),"let iter_op = (m : register) : unit => {\n  let predicate = ([i, j] : [address, move]): unit => assert (j[0] > 3);\n  Map.iter (predicate, m);\n};\n"))),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function map : (('key, 'value) -> 'mapped_value) -> map ('key, 'value) -> map ('key, 'mapped_value)"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val map : (('key * 'value) -> 'mapped_value) -> ('key, 'value) map -> ('key, 'mapped_value) map"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let map: ((('key, 'value)) => 'mapped_value, map('key, 'value)) => map('key, 'mapped_value)"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let map: (mapper: (item: ['key, 'value]) => 'mapped_value, map: map<'key, 'value>) => map<'key, 'mapped_value>"),Object(o.b)("p",null,"Applies the mapper function on the key-value pairs of map and builds a new map"),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),"function map_op (const m : register) : register is\n  block {\n    function increment (const _i : address; const j : move) : move is\n      (j.0, j.1 + 1)\n  } with Map.map (increment, m)\n"))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),"let map_op (m : register) : register =\n  let increment = fun (_i,j : address * move) -> j.0, j.1 + 1\n  in Map.map increment m\n"))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),"let map_op = (m : register) : register => {\n  let increment = ((_i,j): (address, move)) : move => (j[0], j[1] + 1);\n  Map.map (increment, m);\n};\n"))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),"let map_op = (m : register) : register => {\n  let increment = ([_i,j] : [address, move]) : move => [j[0], j[1] + 1];\n  return Map.map (increment, m);\n};\n"))),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function fold : (('accumulator -> ('key, 'value) -> 'accumulator) -> map ('key, 'value) -> 'accumulator) -> 'accumulator"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val fold : (('accumulator * ('key * 'value)) -> 'accumulator) -> ('key, 'value) map -> 'accumulator -> 'accumulator"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let fold: ((('accumulator, ('key, 'value)) => 'accumulator), map('key, 'value), 'accumulator) => 'accumulator"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let fold: (iter: ((accumulator: 'accumulator, item: ['key, 'value]) => 'accumulator), map: map<'key, 'value>, accumulator: 'accumulator) => 'accumulator"),Object(o.b)("p",null,"Fold over key-value pairs of a map"),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),"function fold_op (const m : register) : int is\n  block {\n    function folded (const i : int; const j : address * move) : int is\n      i + j.1.1\n  } with Map.fold (folded, m, 5)\n"))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),"let fold_op (m : register) : int =\n  let folded = fun (i,j : int * (address * move)) -> i + j.1.1\n  in Map.fold folded m 5\n"))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),"let fold_op = (m : register) : int => {\n  let folded = ((i,j): (int, (address, move))) => i + j[1][1];\n  Map.fold (folded, m, 5);\n};\n"))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),"let fold_op = (m : register): int => {\n  let folded = ([i,j]: [int, [address, move]]):int => i + j[1][1];\n  return Map.fold (folded, m, 5);\n};\n"))),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function size : map ('key, 'value) -> nat"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val size : ('key, 'value) map -> nat"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let size: map('key, 'value) => nat"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let size: (map: map<'key, 'value>) => nat"),Object(o.b)("p",null,"Returns the number of items in the map."),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),"const size_ : nat = Map.size (moves);\n"))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),"let size_ : nat = Map.size  moves\n"))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),"let size_ : nat = Map.size (moves);\n"))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),"let size_ : nat = Map.size(moves);\n"))),Object(o.b)(p.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function mem : 'key -> map ('key, 'value) -> bool"),Object(o.b)(p.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val mem : 'key -> ('key, 'value) map -> bool"),Object(o.b)(p.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let mem : ('key, map('key, 'value)) => bool"),Object(o.b)(p.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let mem : (key: 'key, map: map<'key, 'value>) => bool"),Object(o.b)("p",null,"Checks if a key exists in the map."),Object(o.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=maps",group:"maps"}),'const found : bool = Map.mem (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), moves);\n'))),Object(o.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=maps",group:"maps"}),'let found : bool = Map.mem ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)  moves\n'))),Object(o.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=maps",group:"maps"}),'let found : bool = Map.mem (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), moves);\n'))),Object(o.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(o.b)("pre",null,Object(o.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=maps",group:"maps"}),'let found : bool = Map.mem (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address),  moves);\n'))))}b.isMDXComponent=!0},457:function(e,a,t){"use strict";t.d(a,"a",(function(){return m})),t.d(a,"b",(function(){return y}));var n=t(0),r=t.n(n);function o(e,a,t){return a in e?Object.defineProperty(e,a,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[a]=t,e}function s(e,a){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);a&&(n=n.filter((function(a){return Object.getOwnPropertyDescriptor(e,a).enumerable}))),t.push.apply(t,n)}return t}function p(e){for(var a=1;a<arguments.length;a++){var t=null!=arguments[a]?arguments[a]:{};a%2?s(Object(t),!0).forEach((function(a){o(e,a,t[a])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):s(Object(t)).forEach((function(a){Object.defineProperty(e,a,Object.getOwnPropertyDescriptor(t,a))}))}return e}function l(e,a){if(null==e)return{};var t,n,r=function(e,a){if(null==e)return{};var t,n,r={},o=Object.keys(e);for(n=0;n<o.length;n++)t=o[n],a.indexOf(t)>=0||(r[t]=e[t]);return r}(e,a);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)t=o[n],a.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(r[t]=e[t])}return r}var c=r.a.createContext({}),i=function(e){var a=r.a.useContext(c),t=a;return e&&(t="function"==typeof e?e(a):p(p({},a),e)),t},m=function(e){var a=i(e.components);return r.a.createElement(c.Provider,{value:a},e.children)},b={inlineCode:"code",wrapper:function(e){var a=e.children;return r.a.createElement(r.a.Fragment,{},a)}},u=r.a.forwardRef((function(e,a){var t=e.components,n=e.mdxType,o=e.originalType,s=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),m=i(t),u=n,y=m["".concat(s,".").concat(u)]||m[u]||b[u]||o;return t?r.a.createElement(y,p(p({ref:a},c),{},{components:t})):r.a.createElement(y,p({ref:a},c))}));function y(e,a){var t=arguments,n=a&&a.mdxType;if("string"==typeof e||n){var o=t.length,s=new Array(o);s[0]=u;var p={};for(var l in a)hasOwnProperty.call(a,l)&&(p[l]=a[l]);p.originalType=e,p.mdxType="string"==typeof e?e:n,s[1]=p;for(var c=2;c<o;c++)s[c]=t[c];return r.a.createElement.apply(null,s)}return r.a.createElement.apply(null,t)}u.displayName="MDXCreateElement"},458:function(e,a,t){"use strict";var n=t(0),r=t.n(n).a.createContext("pascaligo");a.a=r},459:function(e,a,t){"use strict";var n=t(0),r=t.n(n),o=t(458);t.d(a,"a",(function(){return o.a})),a.b=function(e){return r.a.createElement(o.a.Consumer,null,(function(a){return a===e.syntax?e.children:r.a.createElement(r.a.Fragment,null)}))}},460:function(e,a,t){"use strict";t.r(a),t.d(a,"Prism",(function(){return n.a})),t.d(a,"defaultProps",(function(){return s}));var n=t(22),r={plain:{backgroundColor:"#2a2734",color:"#9a86fd"},styles:[{types:["comment","prolog","doctype","cdata","punctuation"],style:{color:"#6c6783"}},{types:["namespace"],style:{opacity:.7}},{types:["tag","operator","number"],style:{color:"#e09142"}},{types:["property","function"],style:{color:"#9a86fd"}},{types:["tag-id","selector","atrule-id"],style:{color:"#eeebff"}},{types:["attr-name"],style:{color:"#c4b9fe"}},{types:["boolean","string","entity","url","attr-value","keyword","control","directive","unit","statement","regex","at-rule","placeholder","variable"],style:{color:"#ffcc99"}},{types:["deleted"],style:{textDecorationLine:"line-through"}},{types:["inserted"],style:{textDecorationLine:"underline"}},{types:["italic"],style:{fontStyle:"italic"}},{types:["important","bold"],style:{fontWeight:"bold"}},{types:["important"],style:{color:"#c4b9fe"}}]},o=t(0),s={Prism:n.a,theme:r};function p(e,a,t){return a in e?Object.defineProperty(e,a,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[a]=t,e}function l(){return(l=Object.assign||function(e){for(var a=1;a<arguments.length;a++){var t=arguments[a];for(var n in t)Object.prototype.hasOwnProperty.call(t,n)&&(e[n]=t[n])}return e}).apply(this,arguments)}var c=/\r\n|\r|\n/,i=function(e){0===e.length?e.push({types:["plain"],content:"",empty:!0}):1===e.length&&""===e[0].content&&(e[0].empty=!0)},m=function(e,a){var t=e.length;return t>0&&e[t-1]===a?e:e.concat(a)},b=function(e,a){var t=e.plain,n=Object.create(null),r=e.styles.reduce((function(e,t){var n=t.languages,r=t.style;return n&&!n.includes(a)||t.types.forEach((function(a){var t=l({},e[a],r);e[a]=t})),e}),n);return r.root=t,r.plain=l({},t,{backgroundColor:null}),r};function u(e,a){var t={};for(var n in e)Object.prototype.hasOwnProperty.call(e,n)&&-1===a.indexOf(n)&&(t[n]=e[n]);return t}var y=function(e){function a(){for(var a=this,t=[],n=arguments.length;n--;)t[n]=arguments[n];e.apply(this,t),p(this,"getThemeDict",(function(e){if(void 0!==a.themeDict&&e.theme===a.prevTheme&&e.language===a.prevLanguage)return a.themeDict;a.prevTheme=e.theme,a.prevLanguage=e.language;var t=e.theme?b(e.theme,e.language):void 0;return a.themeDict=t})),p(this,"getLineProps",(function(e){var t=e.key,n=e.className,r=e.style,o=l({},u(e,["key","className","style","line"]),{className:"token-line",style:void 0,key:void 0}),s=a.getThemeDict(a.props);return void 0!==s&&(o.style=s.plain),void 0!==r&&(o.style=void 0!==o.style?l({},o.style,r):r),void 0!==t&&(o.key=t),n&&(o.className+=" "+n),o})),p(this,"getStyleForToken",(function(e){var t=e.types,n=e.empty,r=t.length,o=a.getThemeDict(a.props);if(void 0!==o){if(1===r&&"plain"===t[0])return n?{display:"inline-block"}:void 0;if(1===r&&!n)return o[t[0]];var s=n?{display:"inline-block"}:{},p=t.map((function(e){return o[e]}));return Object.assign.apply(Object,[s].concat(p))}})),p(this,"getTokenProps",(function(e){var t=e.key,n=e.className,r=e.style,o=e.token,s=l({},u(e,["key","className","style","token"]),{className:"token "+o.types.join(" "),children:o.content,style:a.getStyleForToken(o),key:void 0});return void 0!==r&&(s.style=void 0!==s.style?l({},s.style,r):r),void 0!==t&&(s.key=t),n&&(s.className+=" "+n),s}))}return e&&(a.__proto__=e),a.prototype=Object.create(e&&e.prototype),a.prototype.constructor=a,a.prototype.render=function(){var e=this.props,a=e.Prism,t=e.language,n=e.code,r=e.children,o=this.getThemeDict(this.props),s=a.languages[t];return r({tokens:function(e){for(var a=[[]],t=[e],n=[0],r=[e.length],o=0,s=0,p=[],l=[p];s>-1;){for(;(o=n[s]++)<r[s];){var b=void 0,u=a[s],y=t[s][o];if("string"==typeof y?(u=s>0?u:["plain"],b=y):(u=m(u,y.type),y.alias&&(u=m(u,y.alias)),b=y.content),"string"==typeof b){var g=b.split(c),d=g.length;p.push({types:u,content:g[0]});for(var j=1;j<d;j++)i(p),l.push(p=[]),p.push({types:u,content:g[j]})}else s++,a.push(u),t.push(b),n.push(0),r.push(b.length)}s--,a.pop(),t.pop(),n.pop(),r.pop()}return i(p),l}(void 0!==s?a.tokenize(n,s,t):[n]),className:"prism-code language-"+t,style:void 0!==o?o.root:{},getLineProps:this.getLineProps,getTokenProps:this.getTokenProps})},a}(o.Component);a.default=y},461:function(e,a,t){"use strict";var n=t(0),r=t(463);a.a=function(){var e=Object(n.useContext)(r.a);if(null==e)throw new Error("`useThemeContext` is used outside of `Layout` Component. See https://v2.docusaurus.io/docs/theme-classic#usethemecontext.");return e}},462:function(e,a,t){"use strict";a.a={plain:{color:"#bfc7d5",backgroundColor:"#292d3e"},styles:[{types:["comment"],style:{color:"rgb(105, 112, 152)",fontStyle:"italic"}},{types:["string","inserted"],style:{color:"rgb(195, 232, 141)"}},{types:["number"],style:{color:"rgb(247, 140, 108)"}},{types:["builtin","char","constant","function"],style:{color:"rgb(130, 170, 255)"}},{types:["punctuation","selector"],style:{color:"rgb(199, 146, 234)"}},{types:["variable"],style:{color:"rgb(191, 199, 213)"}},{types:["class-name","attr-name"],style:{color:"rgb(255, 203, 107)"}},{types:["tag","deleted"],style:{color:"rgb(255, 85, 114)"}},{types:["operator"],style:{color:"rgb(137, 221, 255)"}},{types:["boolean"],style:{color:"rgb(255, 88, 116)"}},{types:["keyword"],style:{fontStyle:"italic"}},{types:["doctype"],style:{color:"rgb(199, 146, 234)",fontStyle:"italic"}},{types:["namespace"],style:{color:"rgb(178, 204, 214)"}},{types:["url"],style:{color:"rgb(221, 221, 221)"}}]}},463:function(e,a,t){"use strict";var n=t(0),r=t.n(n).a.createContext(void 0);a.a=r},464:function(e,a,t){"use strict";var n=t(0),r=t.n(n),o=t(460),s=t(23),p=t(461),l=t(459),c=t(462);function i(){return(i=Object.assign||function(e){for(var a=1;a<arguments.length;a++){var t=arguments[a];for(var n in t)Object.prototype.hasOwnProperty.call(t,n)&&(e[n]=t[n])}return e}).apply(this,arguments)}var m=t(460).Prism;m.languages=Object.assign({},m.languages,{pascaligo:{comment:[/\(\*[\s\S]+?\*\)/,/\/\/.*/],string:{pattern:/(?:'(?:''|[^'\r\n])*'|#[&$%]?[a-f\d]+)+|\^[a-z]/i,greedy:!0},keyword:[{pattern:/(^|[^&])\b(?:absolute|array|asm|begin|case|const|constructor|destructor|do|downto|else|end|file|for|function|goto|if|implementation|inherited|inline|interface|label|nil|object|of|operator|packed|procedure|program|record|reintroduce|repeat|self|set|string|then|to|type|unit|until|uses|var|while|with)\b/i,lookbehind:!0},{pattern:/(^|[^&])\b(?:dispose|exit|false|new|true)\b/i,lookbehind:!0},{pattern:/(^|[^&])\b(?:class|dispinterface|except|exports|finalization|finally|initialization|inline|library|on|out|packed|property|raise|resourcestring|threadvar|try)\b/i,lookbehind:!0},{pattern:/(^|[^&])\b(?:absolute|abstract|alias|assembler|bitpacked|break|cdecl|continue|cppdecl|cvar|default|deprecated|dynamic|enumerator|experimental|export|external|far|far16|forward|generic|helper|implements|index|interrupt|iochecks|local|message|name|near|nodefault|noreturn|nostackframe|oldfpccall|otherwise|overload|override|pascal|platform|private|protected|public|published|read|register|reintroduce|result|safecall|saveregisters|softfloat|specialize|static|stdcall|stored|strict|unaligned|unimplemented|varargs|virtual|write)\b/i,lookbehind:!0}],number:[/(?:[&%]\d+|\$[a-f\d]+)/i,/\b\d+(?:\.\d+)?(?:e[+-]?\d+)?/i],operator:[/\.\.|\*\*|:=|<[<=>]?|>[>=]?|[+\-*\/]=?|[@^=]/i,{pattern:/(^|[^&])\b(?:and|as|div|exclude|in|include|is|mod|not|or|shl|shr|xor)\b/,lookbehind:!0}],punctuation:/\(\.|\.\)|[()\[\]:;,.]/},reasonligo:Object.assign({},m.languages.reason,{comment:[/(^|[^\\])\/\*[\s\S]*?\*\//,/\(\*[\s\S]*?\*\)/,/\/\/.*/]}),cameligo:Object.assign({},m.languages.ocaml,{comment:[/(^|[^\\])\/\*[\s\S]*?\*\//,/\(\*[\s\S]*?\*\)/,/\/\/.*/]}),jsligo:m.languages.typescript}),a.a=function(e){var a=Object(s.default)().siteConfig.themeConfig.prism,t=void 0===a?{}:a,m=Object(p.a)().isDarkTheme,b=t.theme||c.a,u=t.darkTheme||b,y=m?u:b,g=Object(n.useState)(!1),d=g[0],j=g[1];return Object(n.useEffect)((function(){j(!0)}),[]),r.a.createElement(l.a.Consumer,null,(function(a){return a===e.syntax?r.a.createElement(o.default,i({},o.defaultProps,{key:d,language:e.syntax,code:e.children,theme:y}),(function(e){var a=e.className,t=(e.style,e.tokens),n=e.getLineProps,o=e.getTokenProps;return r.a.createElement("pre",{className:a,style:{backgroundColor:"var(--ifm-background-color)",fontSize:"1.1rem",fontWeight:"bold",padding:0,whiteSpace:"break-spaces",marginTop:"3rem"}},t.map((function(e,a){return r.a.createElement("div",n({line:e,key:a}),e.map((function(e,a){return r.a.createElement("span",o({token:e,key:a}))})))})))})):r.a.createElement("div",null)}))}}}]);