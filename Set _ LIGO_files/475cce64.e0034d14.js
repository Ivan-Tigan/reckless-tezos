(window.webpackJsonp=window.webpackJsonp||[]).push([[112],{186:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return c})),n.d(t,"metadata",(function(){return i})),n.d(t,"toc",(function(){return p})),n.d(t,"default",(function(){return b}));var a=n(3),r=n(7),s=(n(0),n(457)),o=n(459),c={id:"types",title:"Types"},i={unversionedId:"language-basics/types",id:"version-0.25.0/language-basics/types",isDocsHomePage:!1,title:"Types",description:"LIGO is strongly and statically typed. This means that the compiler",source:"@site/versioned_docs/version-0.25.0/language-basics/types.md",slug:"/language-basics/types",permalink:"/docs/language-basics/types",version:"0.25.0",sidebar:"version-0.25.0/docs",previous:{title:"Editor Support",permalink:"/docs/intro/editor-support"},next:{title:"Constants & Variables",permalink:"/docs/language-basics/constants-and-variables"}},p=[{value:"Built-in types",id:"built-in-types",children:[]},{value:"Type aliases",id:"type-aliases",children:[]},{value:"Simple types",id:"simple-types",children:[]},{value:"Structured types",id:"structured-types",children:[]},{value:"Annotations",id:"annotations",children:[]}],l={toc:p};function b(e){var t=e.components,n=Object(r.a)(e,["components"]);return Object(s.b)("wrapper",Object(a.a)({},l,n,{components:t,mdxType:"MDXLayout"}),Object(s.b)("p",null,Object(s.b)("em",{parentName:"p"},"LIGO is strongly and statically typed.")," This means that the compiler\nchecks how your contract processes data, ensuring that each function's\nexpectations are met. If it passes the test, your contract will not fail at\nrun-time due to some inconsistent assumptions on your data. This is\ncalled ",Object(s.b)("em",{parentName:"p"},"type checking"),"."),Object(s.b)("p",null,"LIGO types are built on top of Michelson's type system."),Object(s.b)("h2",{id:"built-in-types"},"Built-in types"),Object(s.b)("p",null,"For quick reference, you can find all the built-in types ",Object(s.b)("a",Object(a.a)({parentName:"p"},{href:"https://gitlab.com/ligolang/ligo/-/blob/dev/src/environment/environment.ml"}),"here"),"."),Object(s.b)("h2",{id:"type-aliases"},"Type aliases"),Object(s.b)("p",null,Object(s.b)("em",{parentName:"p"},"Type aliasing")," consists of renaming a given type when the context\ncalls for a more precise name. This increases readability and\nmaintainability of your smart contracts. For example we can choose to\nalias a string type as an animal breed - this will allow us to\ncommunicate our intent with added clarity."),Object(s.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=a",group:"a"}),'type breed is string\nconst dog_breed : breed = "Saluki"\n'))),Object(s.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=a",group:"a"}),'type breed = string\nlet dog_breed : breed = "Saluki"\n'))),Object(s.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=a",group:"a"}),'type breed = string;\nlet dog_breed : breed = "Saluki";\n'))),Object(s.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=a",group:"a"}),'type breed = string;\nlet dog_breed: breed = "Saluki";\n'))),Object(s.b)("blockquote",null,Object(s.b)("p",{parentName:"blockquote"},"The above type definitions are aliases, which means that ",Object(s.b)("inlineCode",{parentName:"p"},"breed")," and\n",Object(s.b)("inlineCode",{parentName:"p"},"string")," are interchangeable in all contexts.")),Object(s.b)("h2",{id:"simple-types"},"Simple types"),Object(s.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=b",group:"b"}),'// The type account_balances denotes maps from addresses to tez\n\ntype account_balances is map (address, tez)\n\nconst ledger : account_balances =\n  map [("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> 10mutez]\n'))),Object(s.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=b",group:"b"}),'// The type account_balances denotes maps from addresses to tez\n\ntype account_balances = (address, tez) map\n\nlet ledger : account_balances =\n  Map.literal\n    [(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), 10mutez)]\n'))),Object(s.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=b",group:"b"}),'// The type account_balances denotes maps from addresses to tez\n\ntype account_balances = map (address, tez);\n\nlet ledger: account_balances =\n  Map.literal\n    ([("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, 10mutez)]);\n'))),Object(s.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=b",group:"b"}),'// The type account_balances denotes maps from addresses to tez\n\ntype account_balances = map<address, tez>;\n\nlet ledger: account_balances =\n  Map.literal\n    (list([["tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address, 10 as mutez]]));\n'))),Object(s.b)("h2",{id:"structured-types"},"Structured types"),Object(s.b)("p",null,"Often contracts require complex data structures, which in turn require\nwell-typed storage or functions to work with. LIGO offers a simple way\nto compose simple types into ",Object(s.b)("em",{parentName:"p"},"structured types"),"."),Object(s.b)("p",null,"The first of those structured types is the ",Object(s.b)("em",{parentName:"p"},"record"),", which aggregates\ntypes as ",Object(s.b)("em",{parentName:"p"},"fields")," and indexes them with a ",Object(s.b)("em",{parentName:"p"},"field name"),". In the example\nbelow you can see the definition of data types for a ledger that keeps\nthe balance and number of previous transactions for a given account."),Object(s.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=c",group:"c"}),'// Type aliasing\n\ntype account is address\ntype number_of_transactions is nat\n\n// The type account_data is a record with two fields.\n\ntype account_data is record [\n  balance : tez;\n  transactions : number_of_transactions\n]\n\n// A ledger is a map from accounts to account_data\n\ntype ledger is map (account, account_data)\n\nconst my_ledger : ledger = map [\n  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) ->\n  record [\n    balance = 10mutez;\n    transactions = 5n\n  ]\n]\n'))),Object(s.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=c",group:"c"}),'// Type aliasing\n\ntype account = address\ntype number_of_transactions = nat\n\n// The type account_data is a record with two fields.\n\ntype account_data = {\n  balance : tez;\n  transactions : number_of_transactions\n}\n\n// A ledger is a map from accounts to account_data\n\ntype ledger = (account, account_data) map\n\nlet my_ledger : ledger = Map.literal\n  [(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address),\n    {balance = 10mutez; transactions = 5n})]\n'))),Object(s.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=c",group:"c"}),'// Type aliasing\n\ntype account = address;\ntype number_of_transactions = nat;\n\n// The type account_data is a record with two fields.\n\ntype account_data = {\n  balance : tez,\n  transactions : number_of_transactions\n};\n\n// A ledger is a map from accounts to account_data\n\ntype ledger = map (account, account_data);\n\nlet my_ledger : ledger =\n  Map.literal([\n    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address,\n     {balance: 10mutez, transactions: 5n})]);\n'))),Object(s.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=c",group:"c"}),'// Type aliasing\n\ntype account = address;\ntype number_of_transactions = nat;\n\n// The type account_data is a record with two fields.\n\ntype account_data = {\n  balance: tez,\n  transactions: number_of_transactions\n};\n\n// A ledger is a map from accounts to account_data\n\ntype ledger = map <account, account_data>;\n\nlet my_ledger : ledger =\n  Map.literal(list([\n    ["tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address,\n     {balance: 10 as mutez, transactions: 5 as nat}]]));\n'))),Object(s.b)("p",null,"Complementary to records are the ",Object(s.b)("em",{parentName:"p"},"variant types"),", which are described in the\nsection on ",Object(s.b)("a",Object(a.a)({parentName:"p"},{href:"https://ligolang.org/docs/language-basics/unit-option-pattern-matching#variant-types"}),"pattern matching"),".\nRecords are a product of types, while variant types are sums of types."),Object(s.b)("h2",{id:"annotations"},"Annotations"),Object(s.b)("p",null,"In certain cases, the type of an expression cannot be properly\ninferred by the compiler. In order to help the type checker, you can\nannotate an expression with its desired type. Here is an example:"),Object(s.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=d",group:"d"}),'type parameter is Back | Claim | Withdraw\n\ntype storage is\n  record\n    owner    : address;\n    goal     : tez;\n    deadline : timestamp;\n    backers  : map (address, tez);\n    funded   : bool\n  end\n\ntype return is list (operation) * storage\n\nfunction back (var action : unit; var store : storage) : return is\n  begin\n    if now > store.deadline then\n      failwith ("Deadline passed.")\n    else case store.backers[sender] of\n           None -> store.backers[sender] := amount\n         | Some (x) -> skip\n         end\n  end with ((nil : list (operation)), store) // Annotation\n'))),Object(s.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=d",group:"d"}),'type parameter = Back | Claim | Withdraw\n\ntype storage = {\n  owner    : address;\n  goal     : tez;\n  deadline : timestamp;\n  backers  : (address, tez) map;\n  funded   : bool\n}\n\ntype return = operation list * storage\n\nlet back (param, store : unit * storage) : return =\n  let no_op : operation list = [] in\n  if Tezos.now > store.deadline then\n    (failwith "Deadline passed." : return) // Annotation\n  else\n    match Map.find_opt sender store.backers with\n      None ->\n        let backers = Map.update sender (Some amount) store.backers\n        in no_op, {store with backers=backers}\n    | Some (x) -> no_op, store\n'))),Object(s.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=d",group:"d"}),'type parameter = | Back | Claim | Withdraw;\n\ntype storage = {\n  owner    : address,\n  goal     : tez,\n  deadline : timestamp,\n  backers  : map (address, tez),\n  funded   : bool,\n};\n\ntype return = (list (operation), storage);\n\nlet back = ((param, store) : (unit, storage)) : return => {\n  let no_op : list (operation) = [];\n  if (Tezos.now > store.deadline) {\n    (failwith ("Deadline passed.") : return); // Annotation\n  }\n  else {\n    switch (Map.find_opt (sender, store.backers)) {\n    | None => {\n        let backers = Map.update (sender, Some (amount), store.backers);\n        (no_op, {...store, backers:backers}) }\n    | Some (x) => (no_op, store)\n    }\n  }\n};\n'))),Object(s.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(s.b)("pre",null,Object(s.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=d",group:"d"}),'type parameter = \n  ["Back"] \n| ["Claim"] \n| ["Withdraw"];\n\ntype storage = {\n  owner    : address,\n  goal     : tez,\n  deadline : timestamp,\n  backers  : map<address, tez>,\n  funded   : bool\n};\n\ntype return_ = [list<operation>, storage];\n\nlet back = ([param, store] : [unit, storage]) : return_ => {\n  let no_op : list<operation> = list([]);\n  if (Tezos.now > store.deadline) {\n    failwith ("Deadline passed.") as return_; // Annotation\n  }\n  else {\n    return match(Map.find_opt (sender, store.backers), {\n      None: () => {\n        let backers = Map.update(sender, Some(amount), store.backers);\n        return [no_op, {...store, backers:backers}]; \n      },\n      Some: (x: tez) => [no_op, store]\n    })\n  };\n};\n'))))}b.isMDXComponent=!0},457:function(e,t,n){"use strict";n.d(t,"a",(function(){return b})),n.d(t,"b",(function(){return m}));var a=n(0),r=n.n(a);function s(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function c(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){s(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function i(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},s=Object.keys(e);for(a=0;a<s.length;a++)n=s[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var s=Object.getOwnPropertySymbols(e);for(a=0;a<s.length;a++)n=s[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var p=r.a.createContext({}),l=function(e){var t=r.a.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):c(c({},t),e)),n},b=function(e){var t=l(e.components);return r.a.createElement(p.Provider,{value:t},e.children)},d={inlineCode:"code",wrapper:function(e){var t=e.children;return r.a.createElement(r.a.Fragment,{},t)}},u=r.a.forwardRef((function(e,t){var n=e.components,a=e.mdxType,s=e.originalType,o=e.parentName,p=i(e,["components","mdxType","originalType","parentName"]),b=l(n),u=a,m=b["".concat(o,".").concat(u)]||b[u]||d[u]||s;return n?r.a.createElement(m,c(c({ref:t},p),{},{components:n})):r.a.createElement(m,c({ref:t},p))}));function m(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var s=n.length,o=new Array(s);o[0]=u;var c={};for(var i in t)hasOwnProperty.call(t,i)&&(c[i]=t[i]);c.originalType=e,c.mdxType="string"==typeof e?e:a,o[1]=c;for(var p=2;p<s;p++)o[p]=n[p];return r.a.createElement.apply(null,o)}return r.a.createElement.apply(null,n)}u.displayName="MDXCreateElement"},458:function(e,t,n){"use strict";var a=n(0),r=n.n(a).a.createContext("pascaligo");t.a=r},459:function(e,t,n){"use strict";var a=n(0),r=n.n(a),s=n(458);n.d(t,"a",(function(){return s.a})),t.b=function(e){return r.a.createElement(s.a.Consumer,null,(function(t){return t===e.syntax?e.children:r.a.createElement(r.a.Fragment,null)}))}}}]);