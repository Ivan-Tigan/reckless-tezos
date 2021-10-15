(window.webpackJsonp=window.webpackJsonp||[]).push([[371],{444:function(e,n,t){"use strict";t.r(n),t.d(n,"frontMatter",(function(){return r})),t.d(n,"metadata",(function(){return b})),t.d(n,"toc",(function(){return c})),t.d(n,"default",(function(){return p}));var a=t(3),i=t(7),l=(t(0),t(457)),o=t(459),r={id:"math-numbers-tez",title:"Math, Numbers & Tez"},b={unversionedId:"language-basics/math-numbers-tez",id:"version-0.25.0/language-basics/math-numbers-tez",isDocsHomePage:!1,title:"Math, Numbers & Tez",description:"LIGO offers three built-in numerical types: int, nat and",source:"@site/versioned_docs/version-0.25.0/language-basics/math-numbers-tez.md",slug:"/language-basics/math-numbers-tez",permalink:"/docs/language-basics/math-numbers-tez",version:"0.25.0",sidebar:"version-0.25.0/docs",previous:{title:"Constants & Variables",permalink:"/docs/language-basics/constants-and-variables"},next:{title:"Strings",permalink:"/docs/language-basics/strings"}},c=[{value:"Addition",id:"addition",children:[]},{value:"Subtraction",id:"subtraction",children:[]},{value:"Multiplication",id:"multiplication",children:[]},{value:"Euclidean Division",id:"euclidean-division",children:[]},{value:"From <code>int</code> to <code>nat</code> and back",id:"from-int-to-nat-and-back",children:[]},{value:"Checking a <code>nat</code>",id:"checking-a-nat",children:[]},{value:"Bitwise operations",id:"bitwise-operations",children:[]},{value:"Bitwise operations",id:"bitwise-operations-1",children:[]}],s={toc:c};function p(e){var n=e.components,t=Object(i.a)(e,["components"]);return Object(l.b)("wrapper",Object(a.a)({},s,t,{components:n,mdxType:"MDXLayout"}),Object(l.b)("p",null,"LIGO offers three built-in numerical types: ",Object(l.b)("inlineCode",{parentName:"p"},"int"),", ",Object(l.b)("inlineCode",{parentName:"p"},"nat")," and\n",Object(l.b)("inlineCode",{parentName:"p"},"tez"),". Values of type ",Object(l.b)("inlineCode",{parentName:"p"},"int")," are integers; values of type ",Object(l.b)("inlineCode",{parentName:"p"},"nat")," are\nnatural numbers (integral numbers greater than or equal to zero);\nvalues of type ",Object(l.b)("inlineCode",{parentName:"p"},"tez")," are units of measure of Tezos tokens."),Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},"Integer literals are the same found in mainstream programming\nlanguages, for example, ",Object(l.b)("inlineCode",{parentName:"li"},"10"),", ",Object(l.b)("inlineCode",{parentName:"li"},"-6")," and ",Object(l.b)("inlineCode",{parentName:"li"},"0"),", but there is only one\ncanonical zero: ",Object(l.b)("inlineCode",{parentName:"li"},"0")," (so, for instance, ",Object(l.b)("inlineCode",{parentName:"li"},"-0")," and ",Object(l.b)("inlineCode",{parentName:"li"},"00")," are invalid).")),Object(l.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},Object(l.b)("p",{parentName:"li"},"Natural numbers are written as digits followed by the suffix ",Object(l.b)("inlineCode",{parentName:"p"},"n"),",\nlike so: ",Object(l.b)("inlineCode",{parentName:"p"},"12n"),", ",Object(l.b)("inlineCode",{parentName:"p"},"0n"),", and the same restriction on zero as integers\napplies: ",Object(l.b)("inlineCode",{parentName:"p"},"0n")," is the only way to specify the natural zero.")),Object(l.b)("li",{parentName:"ul"},Object(l.b)("p",{parentName:"li"},"Tezos tokens can be specified using literals of three kinds:"),Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"units of millionth of ",Object(l.b)("inlineCode",{parentName:"li"},"tez"),", using the suffix ",Object(l.b)("inlineCode",{parentName:"li"},"mutez")," after a\nnatural literal, like ",Object(l.b)("inlineCode",{parentName:"li"},"10000mutez")," or ",Object(l.b)("inlineCode",{parentName:"li"},"0mutez"),";"),Object(l.b)("li",{parentName:"ul"},"units of ",Object(l.b)("inlineCode",{parentName:"li"},"tez"),", using the suffix ",Object(l.b)("inlineCode",{parentName:"li"},"tz")," or ",Object(l.b)("inlineCode",{parentName:"li"},"tez"),", like ",Object(l.b)("inlineCode",{parentName:"li"},"3tz")," or\n",Object(l.b)("inlineCode",{parentName:"li"},"3tez"),";"),Object(l.b)("li",{parentName:"ul"},"decimal amounts of ",Object(l.b)("inlineCode",{parentName:"li"},"tz")," or ",Object(l.b)("inlineCode",{parentName:"li"},"tez"),", like ",Object(l.b)("inlineCode",{parentName:"li"},"12.3tz")," or ",Object(l.b)("inlineCode",{parentName:"li"},"12.4tez"),".")))),Object(l.b)("p",null,"Note that large integral values can be expressed using underscores to\nseparate groups of digits, like ",Object(l.b)("inlineCode",{parentName:"p"},"1_000mutez")," or ",Object(l.b)("inlineCode",{parentName:"p"},"0.000_004tez"),".")),Object(l.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},Object(l.b)("p",{parentName:"li"},"Natural numbers are written as digits followed by the suffix ",Object(l.b)("inlineCode",{parentName:"p"},"n"),",\nlike so: ",Object(l.b)("inlineCode",{parentName:"p"},"12n"),", ",Object(l.b)("inlineCode",{parentName:"p"},"0n"),", and the same restriction on zero as integers\napplies: ",Object(l.b)("inlineCode",{parentName:"p"},"0n")," is the only way to specify the natural zero.")),Object(l.b)("li",{parentName:"ul"},Object(l.b)("p",{parentName:"li"},"Tezos tokens can be specified using literals of three kinds:"),Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"units of millionth of ",Object(l.b)("inlineCode",{parentName:"li"},"tez"),", using the suffix ",Object(l.b)("inlineCode",{parentName:"li"},"mutez")," after a\nnatural literal, like ",Object(l.b)("inlineCode",{parentName:"li"},"10000mutez")," or ",Object(l.b)("inlineCode",{parentName:"li"},"0mutez"),";"),Object(l.b)("li",{parentName:"ul"},"units of ",Object(l.b)("inlineCode",{parentName:"li"},"tez"),", using the suffix ",Object(l.b)("inlineCode",{parentName:"li"},"tz")," or ",Object(l.b)("inlineCode",{parentName:"li"},"tez"),", like ",Object(l.b)("inlineCode",{parentName:"li"},"3tz")," or\n",Object(l.b)("inlineCode",{parentName:"li"},"3tez"),";"),Object(l.b)("li",{parentName:"ul"},"decimal amounts of ",Object(l.b)("inlineCode",{parentName:"li"},"tz")," or ",Object(l.b)("inlineCode",{parentName:"li"},"tez"),", like ",Object(l.b)("inlineCode",{parentName:"li"},"12.3tz")," or ",Object(l.b)("inlineCode",{parentName:"li"},"12.4tez"),".")))),Object(l.b)("p",null,"Note that large integral values can be expressed using underscores to\nseparate groups of digits, like ",Object(l.b)("inlineCode",{parentName:"p"},"1_000mutez")," or ",Object(l.b)("inlineCode",{parentName:"p"},"0.000_004tez"),".")),Object(l.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("ul",null,Object(l.b)("li",{parentName:"ul"},Object(l.b)("p",{parentName:"li"},"Natural numbers are written as digits followed by the annotation ",Object(l.b)("inlineCode",{parentName:"p"},"as nat"),",\nlike so: ",Object(l.b)("inlineCode",{parentName:"p"},"12 as nat"),", ",Object(l.b)("inlineCode",{parentName:"p"},"0 as nat"),", and the same restriction on zero as\nintegers applies: ",Object(l.b)("inlineCode",{parentName:"p"},"0 as nat")," is the only way to specify the natural zero.")),Object(l.b)("li",{parentName:"ul"},Object(l.b)("p",{parentName:"li"},"Tezos tokens can be specified using literals of three kinds:"),Object(l.b)("ul",{parentName:"li"},Object(l.b)("li",{parentName:"ul"},"units of millionth of ",Object(l.b)("inlineCode",{parentName:"li"},"tez"),", using the annotation ",Object(l.b)("inlineCode",{parentName:"li"},"as mutez")," after a\nnatural literal, like ",Object(l.b)("inlineCode",{parentName:"li"},"10000 as mutez")," or ",Object(l.b)("inlineCode",{parentName:"li"},"0 as mutez"),";"),Object(l.b)("li",{parentName:"ul"},"units of ",Object(l.b)("inlineCode",{parentName:"li"},"tez"),", using the annotation ",Object(l.b)("inlineCode",{parentName:"li"},"as tez"),", like ",Object(l.b)("inlineCode",{parentName:"li"},"3 as tez"),";"),Object(l.b)("li",{parentName:"ul"},"decimal amounts of ",Object(l.b)("inlineCode",{parentName:"li"},"tez")," are not supported by JsLIGO, instead the\namount should be written as ",Object(l.b)("inlineCode",{parentName:"li"},"mutez"),".")))),Object(l.b)("p",null,"Note that large integral values can be expressed using underscores to\nseparate groups of digits, like ",Object(l.b)("inlineCode",{parentName:"p"},"1_000 as mutez"),".")),Object(l.b)("h2",{id:"addition"},"Addition"),Object(l.b)("p",null,"Addition in LIGO is accomplished by means of the ",Object(l.b)("inlineCode",{parentName:"p"},"+")," infix\noperator. Some type constraints apply, for example you cannot add a\nvalue of type ",Object(l.b)("inlineCode",{parentName:"p"},"tez")," to a value of type ",Object(l.b)("inlineCode",{parentName:"p"},"nat"),"."),Object(l.b)("p",null,"In the following example you can find a series of arithmetic\noperations, including various numerical types. However, some bits\nremain in comments as they would otherwise not compile, for example,\nadding a value of type ",Object(l.b)("inlineCode",{parentName:"p"},"int")," to a value of type ",Object(l.b)("inlineCode",{parentName:"p"},"tez")," is invalid. Note\nthat adding an integer to a natural number produces an integer."),Object(l.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=a",group:"a"}),"// int + int yields int\nconst a : int = 5 + 10\n\n// nat + int yields int\nconst b : int = 5n + 10\n\n// tez + tez yields tez\nconst c : tez = 5mutez + 0.000_010tez\n\n//tez + int or tez + nat is invalid\n// const d : tez = 5mutez + 10n\n\n// two nats yield a nat\nconst e : nat = 5n + 10n\n\n// nat + int yields an int: invalid\n// const f : nat = 5n + 10;\n\nconst g : int = 1_000_000\n")),Object(l.b)("blockquote",null,Object(l.b)("p",{parentName:"blockquote"},"Pro tip: you can use underscores for readability when defining large\nnumbers:"),Object(l.b)("pre",{parentName:"blockquote"},Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo"}),"const sum : tez = 100_000mutez\n")))),Object(l.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=a",group:"a"}),"// int + int yields int\nlet a : int = 5 + 10\n\n// nat + int yields int\nlet b : int = 5n + 10\n\n// tez + tez yields tez\nlet c : tez = 5mutez + 0.000_010tez\n\n// tez + int or tez + nat is invalid\n// let d : tez = 5mutez + 10n\n\n// two nats yield a nat\nlet e : nat = 5n + 10n\n\n// nat + int yields an int: invalid\n// let f : nat = 5n + 10\n\nlet g : int = 1_000_000\n")),Object(l.b)("blockquote",null,Object(l.b)("p",{parentName:"blockquote"},"Pro tip: you can use underscores for readability when defining large\nnumbers:"),Object(l.b)("pre",{parentName:"blockquote"},Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo"}),"let sum : tez = 100_000mutez\n")))),Object(l.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=a",group:"a"}),"// int + int yields int\nlet a : int = 5 + 10;\n\n// nat + int yields int\nlet b : int = 5n + 10;\n\n// tez + tez yields tez\nlet c : tez = 5mutez + 0.000_010tez;\n\n// tez + int or tez + nat is invalid:\n// let d : tez = 5mutez + 10n;\n\n// two nats yield a nat\nlet e : nat = 5n + 10n;\n\n// nat + int yields an int: invalid\n// let f : nat = 5n + 10;\n\nlet g : int = 1_000_000;\n")),Object(l.b)("blockquote",null,Object(l.b)("p",{parentName:"blockquote"},"Pro tip: you can use underscores for readability when defining large\nnumbers:"),Object(l.b)("pre",{parentName:"blockquote"},Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo"}),"let sum : tez = 100_000mutez;\n")))),Object(l.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=a",group:"a"}),"// int + int yields int\nlet a: int = 5 + 10;\n\n// nat + int yields int\nlet b: int = (5 as nat) + 10;\n\n// tez + tez yields tez\nlet c: tez = (5 as mutez) + (1 as tez);\n\n// tez + int or tez + nat is invalid:\n// let d : tez = (5 as mutez) + (10 as nat);\n\n// two nats yield a nat\nlet e: nat = (5 as nat) + (10 as nat);\n\n// nat + int yields an int: invalid\n// let f : nat = (5 as nat) + 10;\n\nlet g: int = 1_000_000;\n")),Object(l.b)("blockquote",null,Object(l.b)("p",{parentName:"blockquote"},"Pro tip: you can use underscores for readability when defining large\nnumbers:"),Object(l.b)("pre",{parentName:"blockquote"},Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo"}),"let sum : tez = 100_000 as mutez;\n")))),Object(l.b)("h2",{id:"subtraction"},"Subtraction"),Object(l.b)("p",null,"Subtraction looks as follows."),Object(l.b)("blockquote",null,Object(l.b)("p",{parentName:"blockquote"},"\u26a0\ufe0f Even when subtracting two ",Object(l.b)("inlineCode",{parentName:"p"},"nats"),", the result is an ",Object(l.b)("inlineCode",{parentName:"p"},"int"))),Object(l.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=b",group:"b"}),"const a : int = 5 - 10\n\n// Subtraction of two nats yields an int\nconst b : int = 5n - 2n\n\n// Therefore the following is invalid\n// const c : nat = 5n - 2n\n\nconst d : tez = 5mutez - 1mutez\n"))),Object(l.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=b",group:"b"}),"let a : int = 5 - 10\n\n// Subtraction of two nats yields an int\nlet b : int = 5n - 2n\n\n// Therefore the following is invalid\n// let c : nat = 5n - 2n\n\nlet d : tez = 5mutez - 1mutez\n"))),Object(l.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=b",group:"b"}),"let a : int = 5 - 10;\n\n// Subtraction of two nats yields an int\nlet b : int = 5n - 2n;\n\n// Therefore the following is invalid\n// let c : nat = 5n - 2n;\n\nlet d : tez = 5mutez - 1mutez;\n"))),Object(l.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=b",group:"b"}),"let a: int = 5 - 10;\n\n// Subtraction of two nats yields an int\nlet b: int = (5 as nat) - (2 as nat);\n\n// Therefore the following is invalid\n// let c : nat = (5 as nat) - (2 as nat);\n\nlet d: tez = (5 as mutez) - (1 as mutez);\n"))),Object(l.b)("h2",{id:"multiplication"},"Multiplication"),Object(l.b)("p",null,"You can multiply values of the same type, such as:"),Object(l.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=c",group:"c"}),"const a : int = 5 * 5\nconst b : nat = 5n * 5n\n\n// You can also multiply `nat` and `tez`\nconst c : tez = 5n * 5mutez\n"))),Object(l.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=c",group:"c"}),"let a : int = 5 * 5\nlet b : nat = 5n * 5n\n\n// You can also multiply `nat` and `tez`\nlet c : tez = 5n * 5mutez\n"))),Object(l.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=c",group:"c"}),"let a : int = 5 * 5;\nlet b : nat = 5n * 5n;\n\n// You can also multiply `nat` and `tez`\nlet c : tez = 5n * 5mutez;\n"))),Object(l.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=c",group:"c"}),"let a: int = 5 * 5;\nlet b: nat = (5 as nat) * (5 as nat);\n\n// You can also multiply `nat` and `tez`\nlet c: tez = (5 as nat) * (5 as mutez);\n"))),Object(l.b)("h2",{id:"euclidean-division"},"Euclidean Division"),Object(l.b)("p",null,"In LIGO you can divide ",Object(l.b)("inlineCode",{parentName:"p"},"int"),", ",Object(l.b)("inlineCode",{parentName:"p"},"nat"),", and ",Object(l.b)("inlineCode",{parentName:"p"},"tez"),". Here is how:"),Object(l.b)("blockquote",null,Object(l.b)("p",{parentName:"blockquote"},"\u26a0\ufe0f Division of two ",Object(l.b)("inlineCode",{parentName:"p"},"tez")," values results into a ",Object(l.b)("inlineCode",{parentName:"p"},"nat"))),Object(l.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=d",group:"d"}),"const a : int = 10 / 3\nconst b : nat = 10n / 3n\nconst c : nat = 10mutez / 3mutez\n"))),Object(l.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=d",group:"d"}),"let a : int = 10 / 3\nlet b : nat = 10n / 3n\nlet c : nat = 10mutez / 3mutez\n"))),Object(l.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=d",group:"d"}),"let a : int = 10 / 3;\nlet b : nat = 10n / 3n;\nlet c : nat = 10mutez / 3mutez;\n"))),Object(l.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=d",group:"d"}),"let a: int = 10 / 3;\nlet b: nat = (10 as nat) / (3 as nat);\nlet c: nat = (10 as mutez) / (3 as mutez);\n"))),Object(l.b)("p",null,"LIGO also allows you to compute the remainder of the Euclidean\ndivision. In LIGO, it is a natural number."),Object(l.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=d",group:"d"}),"const a : int = 120\nconst b : int = 9\nconst rem1 : nat = a mod b  // 3\nconst c : nat = 120n\nconst rem2 : nat = c mod b  // 3\nconst d : nat = 9n\nconst rem3 : nat = c mod d  // 3\nconst rem4 : nat = a mod d  // 3\n"))),Object(l.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=d",group:"d"}),"let a : int = 120\nlet b : int = 9\nlet rem1 : nat = a mod b  // 3\nlet c : nat = 120n\nlet rem2 : nat = c mod b  // 3\nlet d : nat = 9n\nlet rem3 : nat = c mod d  // 3\nlet rem4 : nat = a mod d  // 3\n"))),Object(l.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=d",group:"d"}),"let a : int = 120;\nlet b : int = 9;\nlet rem1 : nat = a mod b;  // 3\nlet c : nat = 120n;\nlet rem2 : nat = c mod b;  // 3\nlet d : nat = 9n;\nlet rem3 : nat = c mod d;  // 3\nlet rem4 : nat = a mod d;  // 3\n"))),Object(l.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("blockquote",null,Object(l.b)("p",{parentName:"blockquote"},"The behaviour of the ",Object(l.b)("inlineCode",{parentName:"p"},"%")," operator in JsLIGO is different from JavaScript.\nIn JsLIGO, ",Object(l.b)("inlineCode",{parentName:"p"},"%")," is a modulus operator and in JavaScript it's a remainder\noperator. In the case of positive numbers everything is the same, but\nnot with negative numbers.")),Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=d",group:"d"}),"let a: int = 120;\nlet b: int = 9;\nlet rem1: nat = a % b;  // 3\nlet c: nat = 120 as nat;\nlet rem2: nat = c % b;  // 3\nlet d: nat = 9 as nat;\nlet rem3: nat = c % d;  // 3\nlet rem4: nat = a % d;  // 3\n"))),Object(l.b)("p",null,"For cases when you need both the quotient and the remainder, LIGO provides the\n",Object(l.b)("inlineCode",{parentName:"p"},"ediv")," operation. ",Object(l.b)("inlineCode",{parentName:"p"},"ediv x y")," returns ",Object(l.b)("inlineCode",{parentName:"p"},"Some (quotient, remainder)"),", unless ",Object(l.b)("inlineCode",{parentName:"p"},"y"),"\nis zero, in which case it returns ",Object(l.b)("inlineCode",{parentName:"p"},"None")),Object(l.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=f",group:"f"}),"const a : int = 37\nconst b : int = 5\nconst ediv1 : option (int * nat) = ediv(a, b)  // Some (7, 2)\nconst c : nat = 37n\nconst ediv2 : option (int * nat) = ediv(c, b)  // Some (7, 2)\nconst d : nat = 5n\nconst ediv3 : option (nat * nat) = ediv(c, d)  // Some (7, 2)\nconst ediv4 : option (int * nat) = ediv(a, d)  // Some (7, 2)\n"))),Object(l.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=f",group:"f"}),"let a : int = 37\nlet b : int = 5\nlet ediv1 : (int * nat) option = ediv a b  // Some (7, 2)\nlet c : nat = 37n\nlet ediv2 : (int * nat) option = ediv c b  // Some (7, 2)\nlet d : nat = 5n\nlet ediv3 : (nat * nat) option = ediv c d  // Some (7, 2)\nlet ediv4 : (int * nat) option = ediv a d  // Some (7, 2)\n"))),Object(l.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=f",group:"f"}),"let a : int = 37;\nlet b : int = 5;\nlet ediv1 : option((int , nat)) = ediv(a, b);  // Some (7, 2)\nlet c : nat = 37n;\nlet ediv2 : option((int , nat)) = ediv(c, b);  // Some (7, 2)\nlet d : nat = 5n;\nlet ediv3 : option((nat , nat)) = ediv(c, d);  // Some (7, 2)\nlet ediv4 : option((int , nat)) = ediv(a, d);  // Some (7, 2)\n"))),Object(l.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=f",group:"f"}),"let a: int = 37;\nlet b: int = 5;\nlet ediv1 : option<[int , nat]> = ediv(a, b);  // Some (7, 2)\nlet c: nat = 37 as nat;\nlet ediv2: option<[int , nat]> = ediv(c, b);  // Some (7, 2)\nlet d: nat = 5 as nat;\nlet ediv3: option<[nat , nat]> = ediv(c, d);  // Some (7, 2)\nlet ediv4: option<[int , nat]> = ediv(a, d);  // Some (7, 2)\n"))),Object(l.b)("h2",{id:"from-int-to-nat-and-back"},"From ",Object(l.b)("inlineCode",{parentName:"h2"},"int")," to ",Object(l.b)("inlineCode",{parentName:"h2"},"nat")," and back"),Object(l.b)("p",null,"You can ",Object(l.b)("em",{parentName:"p"},"cast")," an ",Object(l.b)("inlineCode",{parentName:"p"},"int")," to a ",Object(l.b)("inlineCode",{parentName:"p"},"nat")," and vice versa. Here is how:"),Object(l.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=e",group:"e"}),"const a : int = int (1n)\nconst b : nat = abs (1)\n"))),Object(l.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=e",group:"e"}),"let a : int = int (1n)\nlet b : nat = abs (1)\n"))),Object(l.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=e",group:"e"}),"let a : int = int (1n);\nlet b : nat = abs (1);\n"))),Object(l.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=e",group:"e"}),"let a: int = int(1 as nat);\nlet b: nat = abs(1);\n"))),Object(l.b)("h2",{id:"checking-a-nat"},"Checking a ",Object(l.b)("inlineCode",{parentName:"h2"},"nat")),Object(l.b)("p",null,"You can check if a value is a ",Object(l.b)("inlineCode",{parentName:"p"},"nat")," by using a predefined cast\nfunction which accepts an ",Object(l.b)("inlineCode",{parentName:"p"},"int")," and returns an optional ",Object(l.b)("inlineCode",{parentName:"p"},"nat"),": if the\nresult is not ",Object(l.b)("inlineCode",{parentName:"p"},"None"),", then the provided integer was indeed a natural\nnumber, and not otherwise."),Object(l.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=e",group:"e"}),"const is_a_nat : option (nat) = is_nat (1)\n"))),Object(l.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=e",group:"e"}),"let is_a_nat : nat option = Michelson.is_nat (1)\n"))),Object(l.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=e",group:"e"}),"let is_a_nat : option (nat) = Michelson.is_nat (1);\n"))),Object(l.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=e",group:"e"}),"let is_a_nat: option<nat> = Michelson.is_nat(1);\n"))),Object(l.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(l.b)("h2",{id:"bitwise-operations"},"Bitwise operations"),Object(l.b)("p",null,"You can perform bitwise operations as follows:"),Object(l.b)("blockquote",null,Object(l.b)("p",{parentName:"blockquote"},"bitwise operations can be performed mostly with ",Object(l.b)("inlineCode",{parentName:"p"},"nat"),"'s"),Object(l.b)("p",{parentName:"blockquote"},"only in case of bitwise and, the first operand can be either ",Object(l.b)("inlineCode",{parentName:"p"},"int")," or ",Object(l.b)("inlineCode",{parentName:"p"},"nat"))),Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo"}),"// Bitwise and (first operand can be int or nat)\nlet four : nat = 4n land 4n // 4\nlet four_ : nat = 7 land 4n // 4\n// Bitwise or \nlet seven : nat = 7n lor 4n // 7\n// Bitwise xor\nlet three : nat = 7n lxor 4n // 3\n// Bitwise shift left\nlet fourteen : nat = 7n lsl 1n // 14\n// Bitwise shift right\nlet seven_ : nat = 14n land 1n // 7\n"))),Object(l.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(l.b)("h2",{id:"bitwise-operations-1"},"Bitwise operations"),Object(l.b)("p",null,"You can perform bitwise operations as follows:"),Object(l.b)("blockquote",null,Object(l.b)("p",{parentName:"blockquote"},"bitwise operations can be performed mostly with ",Object(l.b)("inlineCode",{parentName:"p"},"nat"),"'s"),Object(l.b)("p",{parentName:"blockquote"},"only in case of bitwise and, the first operand can be either ",Object(l.b)("inlineCode",{parentName:"p"},"int")," or ",Object(l.b)("inlineCode",{parentName:"p"},"nat"))),Object(l.b)("pre",null,Object(l.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo"}),"// Bitwise and (first operand can be int or nat)\nlet four : nat = 4n land 4n; // 4\nlet four_ : nat = 7 land 4n; // 4\n// Bitwise or\nlet seven : nat = 7n lor 4n; // 7\n// Bitwise xor\nlet three : nat = 7n lxor 4n; // 3\n// Bitwise shift left\nlet fourteen : nat = 7n lsl 1n; // 14\n// Bitwise shift right\nlet seven_ : nat = 14n land 1n; // 7\n"))))}p.isMDXComponent=!0},457:function(e,n,t){"use strict";t.d(n,"a",(function(){return p})),t.d(n,"b",(function(){return d}));var a=t(0),i=t.n(a);function l(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function o(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function r(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?o(Object(t),!0).forEach((function(n){l(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):o(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function b(e,n){if(null==e)return{};var t,a,i=function(e,n){if(null==e)return{};var t,a,i={},l=Object.keys(e);for(a=0;a<l.length;a++)t=l[a],n.indexOf(t)>=0||(i[t]=e[t]);return i}(e,n);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(a=0;a<l.length;a++)t=l[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(i[t]=e[t])}return i}var c=i.a.createContext({}),s=function(e){var n=i.a.useContext(c),t=n;return e&&(t="function"==typeof e?e(n):r(r({},n),e)),t},p=function(e){var n=s(e.components);return i.a.createElement(c.Provider,{value:n},e.children)},m={inlineCode:"code",wrapper:function(e){var n=e.children;return i.a.createElement(i.a.Fragment,{},n)}},u=i.a.forwardRef((function(e,n){var t=e.components,a=e.mdxType,l=e.originalType,o=e.parentName,c=b(e,["components","mdxType","originalType","parentName"]),p=s(t),u=a,d=p["".concat(o,".").concat(u)]||p[u]||m[u]||l;return t?i.a.createElement(d,r(r({ref:n},c),{},{components:t})):i.a.createElement(d,r({ref:n},c))}));function d(e,n){var t=arguments,a=n&&n.mdxType;if("string"==typeof e||a){var l=t.length,o=new Array(l);o[0]=u;var r={};for(var b in n)hasOwnProperty.call(n,b)&&(r[b]=n[b]);r.originalType=e,r.mdxType="string"==typeof e?e:a,o[1]=r;for(var c=2;c<l;c++)o[c]=t[c];return i.a.createElement.apply(null,o)}return i.a.createElement.apply(null,t)}u.displayName="MDXCreateElement"},458:function(e,n,t){"use strict";var a=t(0),i=t.n(a).a.createContext("pascaligo");n.a=i},459:function(e,n,t){"use strict";var a=t(0),i=t.n(a),l=t(458);t.d(n,"a",(function(){return l.a})),n.b=function(e){return i.a.createElement(l.a.Consumer,null,(function(n){return n===e.syntax?e.children:i.a.createElement(i.a.Fragment,null)}))}}}]);