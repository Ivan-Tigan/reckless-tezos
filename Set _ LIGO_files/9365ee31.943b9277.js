(window.webpackJsonp=window.webpackJsonp||[]).push([[231],{305:function(t,e,n){"use strict";n.r(e),n.d(e,"frontMatter",(function(){return o})),n.d(e,"metadata",(function(){return c})),n.d(e,"toc",(function(){return p})),n.d(e,"default",(function(){return m}));var a=n(3),i=n(7),l=(n(0),n(457)),s=n(459),r=n(464),o={id:"list-reference",title:"List",description:"List operations",hide_table_of_contents:!0},c={unversionedId:"reference/list-reference",id:"version-0.25.0/reference/list-reference",isDocsHomePage:!1,title:"List",description:"List operations",source:"@site/versioned_docs/version-0.25.0/reference/list.md",slug:"/reference/list-reference",permalink:"/docs/reference/list-reference",version:"0.25.0",sidebar:"version-0.25.0/docs",previous:{title:"Crypto",permalink:"/docs/reference/crypto-reference"},next:{title:"Map",permalink:"/docs/reference/map-reference"}},p=[],u={toc:p};function m(t){var e=t.components,n=Object(i.a)(t,["components"]);return Object(l.b)("wrapper",Object(a.a)({},u,n,{components:e,mdxType:"MDXLayout"}),Object(l.b)(r.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function length : nat"),Object(l.b)(r.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val length : nat"),Object(l.b)(r.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let length: nat"),Object(l.b)(r.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let length: nat"),Object(l.b)("p",null,"Get the number of elements in a list."),Object(l.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=lists",group:"lists"}),"const xs: list (int) = list [1; 2; 3]\n\nconst length : nat = List.length (xs);\n"))),Object(l.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=lists",group:"lists"}),"let xs : int list = [1; 2; 3]\n\nlet length : nat = List.length xs\n"))),Object(l.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=lists",group:"lists"}),"let xs : list(int) = [1, 2, 3]\n\nlet length : nat = List.length (xs);\n"))),Object(l.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=lists",group:"lists"}),"let xs : list<int> = list([1, 2, 3]);\n\nlet length : nat = List.length (xs);\n"))),Object(l.b)(r.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function size : nat"),Object(l.b)(r.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val size : nat"),Object(l.b)(r.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let size: nat"),Object(l.b)(r.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let size: nat"),Object(l.b)("p",null,"Get the number of elements in a list."),Object(l.b)("p",null,"Synonym for ",Object(l.b)("inlineCode",{parentName:"p"},"List.length"),"."),Object(l.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=lists",group:"lists"}),"const size_ : nat = List.size (xs);\n"))),Object(l.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=lists",group:"lists"}),"let size : nat = List.size xs\n"))),Object(l.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=lists",group:"lists"}),"let size : nat = List.size (xs);\n"))),Object(l.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=lists",group:"lists"}),"let size : nat = List.size (xs);\n"))),Object(l.b)(r.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function head_opt : list ('a) -> option ('a)"),Object(l.b)(r.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val head_opt : 'a list -> 'a option"),Object(l.b)(r.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let head_opt : list('a) => option('a)"),Object(l.b)(r.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let head_opt : (list: list<'a>) => option<'a>"),Object(l.b)("p",null,"Get the head of a list"),Object(l.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=lists",group:"lists"}),"const head_opt : option (int)  = List.head_opt (xs);\n"))),Object(l.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=lists",group:"lists"}),"let head_opt : int option = List.head_opt xs\n"))),Object(l.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=lists",group:"lists"}),"let head_opt : option(int)  = List.head_opt (xs);\n"))),Object(l.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=lists",group:"lists"}),"let head_opt : option<int>  = List.head_opt (xs);\n"))),Object(l.b)(r.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function tail_opt : list ('a) -> option (list ('a))"),Object(l.b)(r.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val tail_opt : 'a list -> 'a list option"),Object(l.b)(r.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let tail_opt : list('a) => option(list('a))"),Object(l.b)(r.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let tail_opt : (list: list<'a>) => option<list<'a>>"),Object(l.b)("p",null,"Get the tail of a list"),Object(l.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=lists",group:"lists"}),"const tail_opt : option(list(int)) = List.tail_opt (xs);\n"))),Object(l.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=lists",group:"lists"}),"let tail_opt : int list option = List.tail_opt xs\n"))),Object(l.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=lists",group:"lists"}),"let tail_opt : option(list(int)) = List.tail_opt (xs);\n"))),Object(l.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=lists",group:"lists"}),"let tail_opt : option<list<int>> = List.tail_opt (xs);\n"))),Object(l.b)(r.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function iter : ('a -> unit) -> list('a) -> unit"),Object(l.b)(r.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val iter : ('a -> unit) -> 'a list -> unit"),Object(l.b)(r.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let iter: (('a => unit), list('a)) => unit"),Object(l.b)(r.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let iter: (iterator: ((item: 'a) => unit), list: list<'a>) => unit"),Object(l.b)("p",null,"Iterate over items in a list."),Object(l.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=lists",group:"lists"}),'function iter_op (const l : list (int)) : unit is\n  block {\n    function iterated (const i : int) : unit is\n      if i > 3 then Unit else (failwith ("Below range.") : unit)\n  } with List.iter (iterated, l)\n')),Object(l.b)("p",null,"Alternatively it's also possible to use ",Object(l.b)("a",Object(a.a)({parentName:"p"},{href:"/docs/language-basics/loops"}),"loops"),".")),Object(l.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=lists",group:"lists"}),"let iter_op (l : int list) : unit =\n  let predicate = fun (i : int) -> assert (i > 3)\n  in List.iter predicate l\n"))),Object(l.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=lists",group:"lists"}),"let iter_op = (l : list (int)) : unit => {\n  let predicate = (i : int) => assert (i > 3);\n  List.iter (predicate, l);\n};\n"))),Object(l.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=lists",group:"lists"}),"let iter_op = (l: list<int>): unit => {\n  let predicate = (i: int): unit => assert(i > 3);\n  List.iter(predicate, l);\n};\n"))),Object(l.b)(r.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function map : ('a -> 'b) -> list('a) -> list('b)"),Object(l.b)(r.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val map : ('a -> 'b) -> 'a list -> 'b list"),Object(l.b)(r.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let map: (('a => 'b), list('a)) => list('b)"),Object(l.b)(r.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let map: (mapper: ((item: 'a) => 'b), list: list<'a>) => list<'b>"),Object(l.b)("p",null,"Apply a function to items of a list to create a new list."),Object(l.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=lists",group:"lists"}),"const larger_list: list(int) = list [1; 2; 3]\n\nfunction increment (const i : int): int is i + 1\n\n// Creates a new list with all elements incremented by 1\nconst plus_one : list (int) = List.map (increment, larger_list)\n"))),Object(l.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=lists",group:"lists"}),"let larger_list: int list = [1; 2; 3]\n\nlet increment (i : int) : int = i + 1\n\n// Creates a new list with all elements incremented by 1\nlet plus_one : int list = List.map increment larger_list\n"))),Object(l.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=lists",group:"lists"}),"let larger_list: list(int) = [1, 2, 3];\n\nlet increment = (i : int) : int => i + 1;\n\n// Creates a new list with all elements incremented by 1\nlet plus_one : list (int) = List.map (increment, larger_list);\n"))),Object(l.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=lists",group:"lists"}),"let larger_list: list<int> = list([1, 2, 3]);\n\nlet increment = (i : int): int => i + 1;\n\n// Creates a new list with all elements incremented by 1\nlet plus_one : list<int> = List.map(increment, larger_list);\n"))),Object(l.b)(r.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function fold : (('accumulator -> 'item -> 'accumulator) -> list('item) -> 'accumulator) -> 'accumulator"),Object(l.b)(r.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val fold : (('accumulator * 'item) -> 'accumulator) -> 'item list -> 'accumulator -> 'accumulator"),Object(l.b)(r.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let fold: ((('accumulator, 'item) => 'accumulator), list('item), 'accumulator) => 'accumulator"),Object(l.b)(r.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let fold: ((folder: [accumulator: 'accumulator, item: 'item]) => 'accumulator, list: list<'item>, accumulator: 'accumulator) => 'accumulator"),Object(l.b)("p",null,Object(l.b)("a",Object(a.a)({parentName:"p"},{href:"/docs/language-basics/sets-lists-tuples#folded-operation-over-lists"}),"Fold over items in a list"),";"),Object(l.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=lists",group:"lists"}),"const my_list: list(int) = list [1; 2; 3]\n\nfunction sum (const acc : int; const i : int): int is acc + i\n\nconst sum_of_elements : int = List.fold (sum, my_list, 0)\n"))),Object(l.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=lists",group:"lists"}),"let my_list : int list = [1; 2; 3]\n\nlet sum (acc, i : int * int) : int = acc + i\n\nlet sum_of_elements : int = List.fold sum my_list 0\n"))),Object(l.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=lists",group:"lists"}),"let my_list : list(int) = [1, 2, 3];\n\nlet sum = ((result, i): (int, int)): int => result + i;\n\nlet sum_of_elements : int = List.fold (sum, my_list, 0);\n"))),Object(l.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=lists",group:"lists"}),"let my_list: list<int> = list([1, 2, 3]);\n\nlet sum = ([result, i]: [int, int]): int => result + i;\n\nlet sum_of_elements: int = List.fold(sum, my_list, 0);\n"))),Object(l.b)(r.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function fold_left : (('accumulator -> 'item -> 'accumulator) -> 'accumulator -> list('item)) -> 'accumulator"),Object(l.b)(r.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val fold_left : (('accumulator * 'item) -> 'accumulator) -> 'accumulator -> 'item list -> 'accumulator"),Object(l.b)(r.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let fold_left: ((('accumulator, 'item) => 'accumulator), 'accumulator, list('item)) => 'accumulator"),Object(l.b)(r.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let fold_left: (((a: ['accumulator, 'item]) => 'accumulator), 'accumulator, list<'item>) => 'accumulator"),Object(l.b)("p",null,Object(l.b)("a",Object(a.a)({parentName:"p"},{href:"/docs/language-basics/sets-lists-tuples#folded-operation-over-lists"}),"Fold over items in a list"),";"),Object(l.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=lists",group:"lists"}),"const my_list: list(int) = list [1; 2; 3]\n\nfunction sum (const acc : int; const i : int): int is acc + i\n\nconst sum_of_elements : int = List.fold_left (sum, 0, my_list)\n"))),Object(l.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=lists",group:"lists"}),"let my_list : int list = [1; 2; 3]\n\nlet sum (acc, i : int * int) : int = acc + i\n\nlet sum_of_elements : int = List.fold_left sum 0 my_list\n"))),Object(l.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=lists",group:"lists"}),"let my_list : list(int) = [1, 2, 3];\n\nlet sum = ((result, i): (int, int)): int => result + i;\n\nlet sum_of_elements : int = List.fold_left (sum, 0, my_list);\n"))),Object(l.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=lists",group:"lists"}),"let my_list : list<int> = list([1, 2, 3]);\n\nlet sum = ([result, i]: [int, int]): int => result + i;\n\nlet sum_of_elements : int = List.fold_left (sum, 0, my_list);\n"))),Object(l.b)(r.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function fold_right : (('item -> 'accumulator -> 'accumulator) -> list('item) -> 'accumulator) -> 'accumulator"),Object(l.b)(r.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val fold_right : (('item * 'accumulator) -> 'accumulator) -> 'item list -> 'accumulator -> 'accumulator"),Object(l.b)(r.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let fold_right: ((('item, 'accumulator) => 'accumulator), list('item), 'accumulator) => 'accumulator"),Object(l.b)(r.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let fold_right: (((a: ['item, 'accumulator]) => 'accumulator), list<'item>, 'accumulator) => 'accumulator"),Object(l.b)("p",null,Object(l.b)("a",Object(a.a)({parentName:"p"},{href:"/docs/language-basics/sets-lists-tuples#folded-operation-over-lists"}),"Fold over items in a list"),";"),Object(l.b)(s.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=lists",group:"lists"}),"const my_list: list(int) = list [1; 2; 3]\n\nfunction sum_right (const i : int; const acc : int): int is acc + i\n\nconst sum_of_elements : int = List.fold_right (sum_right, my_list, 0)\n"))),Object(l.b)(s.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=lists",group:"lists"}),"let my_list : int list = [1; 2; 3]\n\nlet sum_right (i, acc : int * int) : int = acc + i\n\nlet sum_of_elements : int = List.fold_right sum_right my_list 0\n"))),Object(l.b)(s.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=lists",group:"lists"}),"let my_list : list(int) = [1, 2, 3];\n\nlet sum_right = ((i, result): (int, int)): int => result + i;\n\nlet sum_of_elements : int = List.fold_right (sum_right, my_list, 0);\n"))),Object(l.b)(s.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=lists",group:"lists"}),"let my_list : list<int> = list([1, 2, 3]);\n\nlet sum_right = ([i, result]: [int, int]): int => result + i;\n\nlet sum_of_elements : int = List.fold_right (sum_right, my_list, 0);\n"))))}m.isMDXComponent=!0},457:function(t,e,n){"use strict";n.d(e,"a",(function(){return u})),n.d(e,"b",(function(){return g}));var a=n(0),i=n.n(a);function l(t,e,n){return e in t?Object.defineProperty(t,e,{value:n,enumerable:!0,configurable:!0,writable:!0}):t[e]=n,t}function s(t,e){var n=Object.keys(t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(t);e&&(a=a.filter((function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable}))),n.push.apply(n,a)}return n}function r(t){for(var e=1;e<arguments.length;e++){var n=null!=arguments[e]?arguments[e]:{};e%2?s(Object(n),!0).forEach((function(e){l(t,e,n[e])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(n)):s(Object(n)).forEach((function(e){Object.defineProperty(t,e,Object.getOwnPropertyDescriptor(n,e))}))}return t}function o(t,e){if(null==t)return{};var n,a,i=function(t,e){if(null==t)return{};var n,a,i={},l=Object.keys(t);for(a=0;a<l.length;a++)n=l[a],e.indexOf(n)>=0||(i[n]=t[n]);return i}(t,e);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(t);for(a=0;a<l.length;a++)n=l[a],e.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(t,n)&&(i[n]=t[n])}return i}var c=i.a.createContext({}),p=function(t){var e=i.a.useContext(c),n=e;return t&&(n="function"==typeof t?t(e):r(r({},e),t)),n},u=function(t){var e=p(t.components);return i.a.createElement(c.Provider,{value:e},t.children)},m={inlineCode:"code",wrapper:function(t){var e=t.children;return i.a.createElement(i.a.Fragment,{},e)}},b=i.a.forwardRef((function(t,e){var n=t.components,a=t.mdxType,l=t.originalType,s=t.parentName,c=o(t,["components","mdxType","originalType","parentName"]),u=p(n),b=a,g=u["".concat(s,".").concat(b)]||u[b]||m[b]||l;return n?i.a.createElement(g,r(r({ref:e},c),{},{components:n})):i.a.createElement(g,r({ref:e},c))}));function g(t,e){var n=arguments,a=e&&e.mdxType;if("string"==typeof t||a){var l=n.length,s=new Array(l);s[0]=b;var r={};for(var o in e)hasOwnProperty.call(e,o)&&(r[o]=e[o]);r.originalType=t,r.mdxType="string"==typeof t?t:a,s[1]=r;for(var c=2;c<l;c++)s[c]=n[c];return i.a.createElement.apply(null,s)}return i.a.createElement.apply(null,n)}b.displayName="MDXCreateElement"},458:function(t,e,n){"use strict";var a=n(0),i=n.n(a).a.createContext("pascaligo");e.a=i},459:function(t,e,n){"use strict";var a=n(0),i=n.n(a),l=n(458);n.d(e,"a",(function(){return l.a})),e.b=function(t){return i.a.createElement(l.a.Consumer,null,(function(e){return e===t.syntax?t.children:i.a.createElement(i.a.Fragment,null)}))}},460:function(t,e,n){"use strict";n.r(e),n.d(e,"Prism",(function(){return a.a})),n.d(e,"defaultProps",(function(){return s}));var a=n(22),i={plain:{backgroundColor:"#2a2734",color:"#9a86fd"},styles:[{types:["comment","prolog","doctype","cdata","punctuation"],style:{color:"#6c6783"}},{types:["namespace"],style:{opacity:.7}},{types:["tag","operator","number"],style:{color:"#e09142"}},{types:["property","function"],style:{color:"#9a86fd"}},{types:["tag-id","selector","atrule-id"],style:{color:"#eeebff"}},{types:["attr-name"],style:{color:"#c4b9fe"}},{types:["boolean","string","entity","url","attr-value","keyword","control","directive","unit","statement","regex","at-rule","placeholder","variable"],style:{color:"#ffcc99"}},{types:["deleted"],style:{textDecorationLine:"line-through"}},{types:["inserted"],style:{textDecorationLine:"underline"}},{types:["italic"],style:{fontStyle:"italic"}},{types:["important","bold"],style:{fontWeight:"bold"}},{types:["important"],style:{color:"#c4b9fe"}}]},l=n(0),s={Prism:a.a,theme:i};function r(t,e,n){return e in t?Object.defineProperty(t,e,{value:n,enumerable:!0,configurable:!0,writable:!0}):t[e]=n,t}function o(){return(o=Object.assign||function(t){for(var e=1;e<arguments.length;e++){var n=arguments[e];for(var a in n)Object.prototype.hasOwnProperty.call(n,a)&&(t[a]=n[a])}return t}).apply(this,arguments)}var c=/\r\n|\r|\n/,p=function(t){0===t.length?t.push({types:["plain"],content:"",empty:!0}):1===t.length&&""===t[0].content&&(t[0].empty=!0)},u=function(t,e){var n=t.length;return n>0&&t[n-1]===e?t:t.concat(e)},m=function(t,e){var n=t.plain,a=Object.create(null),i=t.styles.reduce((function(t,n){var a=n.languages,i=n.style;return a&&!a.includes(e)||n.types.forEach((function(e){var n=o({},t[e],i);t[e]=n})),t}),a);return i.root=n,i.plain=o({},n,{backgroundColor:null}),i};function b(t,e){var n={};for(var a in t)Object.prototype.hasOwnProperty.call(t,a)&&-1===e.indexOf(a)&&(n[a]=t[a]);return n}var g=function(t){function e(){for(var e=this,n=[],a=arguments.length;a--;)n[a]=arguments[a];t.apply(this,n),r(this,"getThemeDict",(function(t){if(void 0!==e.themeDict&&t.theme===e.prevTheme&&t.language===e.prevLanguage)return e.themeDict;e.prevTheme=t.theme,e.prevLanguage=t.language;var n=t.theme?m(t.theme,t.language):void 0;return e.themeDict=n})),r(this,"getLineProps",(function(t){var n=t.key,a=t.className,i=t.style,l=o({},b(t,["key","className","style","line"]),{className:"token-line",style:void 0,key:void 0}),s=e.getThemeDict(e.props);return void 0!==s&&(l.style=s.plain),void 0!==i&&(l.style=void 0!==l.style?o({},l.style,i):i),void 0!==n&&(l.key=n),a&&(l.className+=" "+a),l})),r(this,"getStyleForToken",(function(t){var n=t.types,a=t.empty,i=n.length,l=e.getThemeDict(e.props);if(void 0!==l){if(1===i&&"plain"===n[0])return a?{display:"inline-block"}:void 0;if(1===i&&!a)return l[n[0]];var s=a?{display:"inline-block"}:{},r=n.map((function(t){return l[t]}));return Object.assign.apply(Object,[s].concat(r))}})),r(this,"getTokenProps",(function(t){var n=t.key,a=t.className,i=t.style,l=t.token,s=o({},b(t,["key","className","style","token"]),{className:"token "+l.types.join(" "),children:l.content,style:e.getStyleForToken(l),key:void 0});return void 0!==i&&(s.style=void 0!==s.style?o({},s.style,i):i),void 0!==n&&(s.key=n),a&&(s.className+=" "+a),s}))}return t&&(e.__proto__=t),e.prototype=Object.create(t&&t.prototype),e.prototype.constructor=e,e.prototype.render=function(){var t=this.props,e=t.Prism,n=t.language,a=t.code,i=t.children,l=this.getThemeDict(this.props),s=e.languages[n];return i({tokens:function(t){for(var e=[[]],n=[t],a=[0],i=[t.length],l=0,s=0,r=[],o=[r];s>-1;){for(;(l=a[s]++)<i[s];){var m=void 0,b=e[s],g=n[s][l];if("string"==typeof g?(b=s>0?b:["plain"],m=g):(b=u(b,g.type),g.alias&&(b=u(b,g.alias)),m=g.content),"string"==typeof m){var y=m.split(c),d=y.length;r.push({types:b,content:y[0]});for(var j=1;j<d;j++)p(r),o.push(r=[]),r.push({types:b,content:y[j]})}else s++,e.push(b),n.push(m),a.push(0),i.push(m.length)}s--,e.pop(),n.pop(),a.pop(),i.pop()}return p(r),o}(void 0!==s?e.tokenize(a,s,n):[a]),className:"prism-code language-"+n,style:void 0!==l?l.root:{},getLineProps:this.getLineProps,getTokenProps:this.getTokenProps})},e}(l.Component);e.default=g},461:function(t,e,n){"use strict";var a=n(0),i=n(463);e.a=function(){var t=Object(a.useContext)(i.a);if(null==t)throw new Error("`useThemeContext` is used outside of `Layout` Component. See https://v2.docusaurus.io/docs/theme-classic#usethemecontext.");return t}},462:function(t,e,n){"use strict";e.a={plain:{color:"#bfc7d5",backgroundColor:"#292d3e"},styles:[{types:["comment"],style:{color:"rgb(105, 112, 152)",fontStyle:"italic"}},{types:["string","inserted"],style:{color:"rgb(195, 232, 141)"}},{types:["number"],style:{color:"rgb(247, 140, 108)"}},{types:["builtin","char","constant","function"],style:{color:"rgb(130, 170, 255)"}},{types:["punctuation","selector"],style:{color:"rgb(199, 146, 234)"}},{types:["variable"],style:{color:"rgb(191, 199, 213)"}},{types:["class-name","attr-name"],style:{color:"rgb(255, 203, 107)"}},{types:["tag","deleted"],style:{color:"rgb(255, 85, 114)"}},{types:["operator"],style:{color:"rgb(137, 221, 255)"}},{types:["boolean"],style:{color:"rgb(255, 88, 116)"}},{types:["keyword"],style:{fontStyle:"italic"}},{types:["doctype"],style:{color:"rgb(199, 146, 234)",fontStyle:"italic"}},{types:["namespace"],style:{color:"rgb(178, 204, 214)"}},{types:["url"],style:{color:"rgb(221, 221, 221)"}}]}},463:function(t,e,n){"use strict";var a=n(0),i=n.n(a).a.createContext(void 0);e.a=i},464:function(t,e,n){"use strict";var a=n(0),i=n.n(a),l=n(460),s=n(23),r=n(461),o=n(459),c=n(462);function p(){return(p=Object.assign||function(t){for(var e=1;e<arguments.length;e++){var n=arguments[e];for(var a in n)Object.prototype.hasOwnProperty.call(n,a)&&(t[a]=n[a])}return t}).apply(this,arguments)}var u=n(460).Prism;u.languages=Object.assign({},u.languages,{pascaligo:{comment:[/\(\*[\s\S]+?\*\)/,/\/\/.*/],string:{pattern:/(?:'(?:''|[^'\r\n])*'|#[&$%]?[a-f\d]+)+|\^[a-z]/i,greedy:!0},keyword:[{pattern:/(^|[^&])\b(?:absolute|array|asm|begin|case|const|constructor|destructor|do|downto|else|end|file|for|function|goto|if|implementation|inherited|inline|interface|label|nil|object|of|operator|packed|procedure|program|record|reintroduce|repeat|self|set|string|then|to|type|unit|until|uses|var|while|with)\b/i,lookbehind:!0},{pattern:/(^|[^&])\b(?:dispose|exit|false|new|true)\b/i,lookbehind:!0},{pattern:/(^|[^&])\b(?:class|dispinterface|except|exports|finalization|finally|initialization|inline|library|on|out|packed|property|raise|resourcestring|threadvar|try)\b/i,lookbehind:!0},{pattern:/(^|[^&])\b(?:absolute|abstract|alias|assembler|bitpacked|break|cdecl|continue|cppdecl|cvar|default|deprecated|dynamic|enumerator|experimental|export|external|far|far16|forward|generic|helper|implements|index|interrupt|iochecks|local|message|name|near|nodefault|noreturn|nostackframe|oldfpccall|otherwise|overload|override|pascal|platform|private|protected|public|published|read|register|reintroduce|result|safecall|saveregisters|softfloat|specialize|static|stdcall|stored|strict|unaligned|unimplemented|varargs|virtual|write)\b/i,lookbehind:!0}],number:[/(?:[&%]\d+|\$[a-f\d]+)/i,/\b\d+(?:\.\d+)?(?:e[+-]?\d+)?/i],operator:[/\.\.|\*\*|:=|<[<=>]?|>[>=]?|[+\-*\/]=?|[@^=]/i,{pattern:/(^|[^&])\b(?:and|as|div|exclude|in|include|is|mod|not|or|shl|shr|xor)\b/,lookbehind:!0}],punctuation:/\(\.|\.\)|[()\[\]:;,.]/},reasonligo:Object.assign({},u.languages.reason,{comment:[/(^|[^\\])\/\*[\s\S]*?\*\//,/\(\*[\s\S]*?\*\)/,/\/\/.*/]}),cameligo:Object.assign({},u.languages.ocaml,{comment:[/(^|[^\\])\/\*[\s\S]*?\*\//,/\(\*[\s\S]*?\*\)/,/\/\/.*/]}),jsligo:u.languages.typescript}),e.a=function(t){var e=Object(s.default)().siteConfig.themeConfig.prism,n=void 0===e?{}:e,u=Object(r.a)().isDarkTheme,m=n.theme||c.a,b=n.darkTheme||m,g=u?b:m,y=Object(a.useState)(!1),d=y[0],j=y[1];return Object(a.useEffect)((function(){j(!0)}),[]),i.a.createElement(o.a.Consumer,null,(function(e){return e===t.syntax?i.a.createElement(l.default,p({},l.defaultProps,{key:d,language:t.syntax,code:t.children,theme:g}),(function(t){var e=t.className,n=(t.style,t.tokens),a=t.getLineProps,l=t.getTokenProps;return i.a.createElement("pre",{className:e,style:{backgroundColor:"var(--ifm-background-color)",fontSize:"1.1rem",fontWeight:"bold",padding:0,whiteSpace:"break-spaces",marginTop:"3rem"}},n.map((function(t,e){return i.a.createElement("div",a({line:t,key:e}),t.map((function(t,e){return i.a.createElement("span",l({token:t,key:e}))})))})))})):i.a.createElement("div",null)}))}}}]);