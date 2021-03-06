(window.webpackJsonp=window.webpackJsonp||[]).push([[7],{457:function(e,t,n){"use strict";n.d(t,"a",(function(){return g})),n.d(t,"b",(function(){return m}));var a=n(0),r=n.n(a);function c(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){c(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},c=Object.keys(e);for(a=0;a<c.length;a++)n=c[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var c=Object.getOwnPropertySymbols(e);for(a=0;a<c.length;a++)n=c[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var b=r.a.createContext({}),o=function(e){var t=r.a.useContext(b),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},g=function(e){var t=o(e.components);return r.a.createElement(b.Provider,{value:t},e.children)},p={inlineCode:"code",wrapper:function(e){var t=e.children;return r.a.createElement(r.a.Fragment,{},t)}},u=r.a.forwardRef((function(e,t){var n=e.components,a=e.mdxType,c=e.originalType,i=e.parentName,b=s(e,["components","mdxType","originalType","parentName"]),g=o(n),u=a,m=g["".concat(i,".").concat(u)]||g[u]||p[u]||c;return n?r.a.createElement(m,l(l({ref:t},b),{},{components:n})):r.a.createElement(m,l({ref:t},b))}));function m(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var c=n.length,i=new Array(c);i[0]=u;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l.mdxType="string"==typeof e?e:a,i[1]=l;for(var b=2;b<c;b++)i[b]=n[b];return r.a.createElement.apply(null,i)}return r.a.createElement.apply(null,n)}u.displayName="MDXCreateElement"},458:function(e,t,n){"use strict";var a=n(0),r=n.n(a).a.createContext("pascaligo");t.a=r},459:function(e,t,n){"use strict";var a=n(0),r=n.n(a),c=n(458);n.d(t,"a",(function(){return c.a})),t.b=function(e){return r.a.createElement(c.a.Consumer,null,(function(t){return t===e.syntax?e.children:r.a.createElement(r.a.Fragment,null)}))}},70:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return l})),n.d(t,"metadata",(function(){return s})),n.d(t,"toc",(function(){return b})),n.d(t,"default",(function(){return g}));var a=n(3),r=n(7),c=(n(0),n(457)),i=n(459),l={id:"strings",title:"Strings"},s={unversionedId:"language-basics/strings",id:"version-0.25.0/language-basics/strings",isDocsHomePage:!1,title:"Strings",description:"Strings are defined using the built-in string type like this:",source:"@site/versioned_docs/version-0.25.0/language-basics/strings.md",slug:"/language-basics/strings",permalink:"/docs/language-basics/strings",version:"0.25.0",sidebar:"version-0.25.0/docs",previous:{title:"Math, Numbers & Tez",permalink:"/docs/language-basics/math-numbers-tez"},next:{title:"Functions",permalink:"/docs/language-basics/functions"}},b=[{value:"Concatenating Strings",id:"concatenating-strings",children:[]},{value:"Extracting Substrings",id:"extracting-substrings",children:[]},{value:"Length of Strings",id:"length-of-strings",children:[]}],o={toc:b};function g(e){var t=e.components,n=Object(r.a)(e,["components"]);return Object(c.b)("wrapper",Object(a.a)({},o,n,{components:t,mdxType:"MDXLayout"}),Object(c.b)("p",null,"Strings are defined using the built-in ",Object(c.b)("inlineCode",{parentName:"p"},"string")," type like this:"),Object(c.b)(i.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{}),'const a : string = "Hello Alice"\n'))),Object(c.b)(i.b,{syntax:"cameligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{}),'let a : string = "Hello Alice"\n'))),Object(c.b)(i.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo"}),'let a : string = "Hello Alice";\n'))),Object(c.b)(i.b,{syntax:"jsligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo"}),'let a: string = "Hello Alice";\n')),Object(c.b)("p",null,"or with single quotes:"),Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo"}),"let a: string = 'Hello Alice';\n"))),Object(c.b)("h2",{id:"concatenating-strings"},"Concatenating Strings"),Object(c.b)(i.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(c.b)("p",null,"Strings can be concatenated using the ",Object(c.b)("inlineCode",{parentName:"p"},"^")," operator."),Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=a",group:"a"}),'const name : string = "Alice"\nconst greeting : string = "Hello"\nconst full_greeting : string = greeting ^ " " ^ name\n'))),Object(c.b)(i.b,{syntax:"cameligo",mdxType:"Syntax"},Object(c.b)("p",null,"Strings can be concatenated using the ",Object(c.b)("inlineCode",{parentName:"p"},"^")," operator."),Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=a",group:"a"}),'let name : string = "Alice"\nlet greeting : string = "Hello"\nlet full_greeting : string = greeting ^ " " ^ name\n'))),Object(c.b)(i.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(c.b)("p",null,"Strings can be concatenated using the ",Object(c.b)("inlineCode",{parentName:"p"},"++")," operator."),Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=a",group:"a"}),'let name : string = "Alice";\nlet greeting : string = "Hello";\nlet full_greeting : string = greeting ++ " " ++ name;\n'))),Object(c.b)(i.b,{syntax:"jsligo",mdxType:"Syntax"},Object(c.b)("p",null,"Strings can be concatenated using the ",Object(c.b)("inlineCode",{parentName:"p"},"+")," operator."),Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=a",group:"a"}),'let name: string = "Alice";\nlet greeting: string = "Hello";\nlet full_greeting: string = greeting + " " + name;\n'))),Object(c.b)("h2",{id:"extracting-substrings"},"Extracting Substrings"),Object(c.b)("p",null,"Substrings can be extracted using the predefined function\n",Object(c.b)("inlineCode",{parentName:"p"},"String.sub"),". The first character has index 0 and the interval of\nindices for the substring has inclusive bounds."),Object(c.b)(i.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=b",group:"b"}),'const name  : string = "Alice"\nconst slice : string = String.sub (0n, 1n, name)\n'))),Object(c.b)(i.b,{syntax:"cameligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=b",group:"b"}),'let name  : string = "Alice"\nlet slice : string = String.sub 0n 1n name\n'))),Object(c.b)(i.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=b",group:"b"}),'let name  : string = "Alice";\nlet slice : string = String.sub (0n, 1n, name);\n'))),Object(c.b)(i.b,{syntax:"jsligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=b",group:"b"}),'let name: string = "Alice";\nlet slice: string = String.sub (0 as nat, 1 as nat, name);\n'))),Object(c.b)("blockquote",null,Object(c.b)("p",{parentName:"blockquote"},"\u26a0\ufe0f Notice that the offset and length of the slice are natural\nnumbers.")),Object(c.b)("h2",{id:"length-of-strings"},"Length of Strings"),Object(c.b)("p",null,"The length of a string can be found using a built-in function:"),Object(c.b)(i.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=c",group:"c"}),'const name : string = "Alice"\nconst length : nat = String.length (name) // length = 5\n')),Object(c.b)("blockquote",null,Object(c.b)("p",{parentName:"blockquote"},"Note that ",Object(c.b)("inlineCode",{parentName:"p"},"size")," is ",Object(c.b)("em",{parentName:"p"},"deprecated"),". "))),Object(c.b)(i.b,{syntax:"cameligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=c",group:"c"}),'let name : string = "Alice"\nlet length : nat = String.length name  // length = 5\n')),Object(c.b)("blockquote",null,Object(c.b)("p",{parentName:"blockquote"},"Note that ",Object(c.b)("inlineCode",{parentName:"p"},"String.size")," is ",Object(c.b)("em",{parentName:"p"},"deprecated"),"."))),Object(c.b)(i.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=c",group:"c"}),'let name : string = "Alice";\nlet length : nat = String.length (name);  // length == 5\n')),Object(c.b)("blockquote",null,Object(c.b)("p",{parentName:"blockquote"},"Note that ",Object(c.b)("inlineCode",{parentName:"p"},"String.size")," is ",Object(c.b)("em",{parentName:"p"},"deprecated"),"."))),Object(c.b)(i.b,{syntax:"jsligo",mdxType:"Syntax"},Object(c.b)("pre",null,Object(c.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=c",group:"c"}),'let name: string = "Alice";\nlet length: nat = String.length(name);  // length == 5\n'))))}g.isMDXComponent=!0}}]);