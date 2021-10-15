(window.webpackJsonp=window.webpackJsonp||[]).push([[125],{199:function(e,n,t){"use strict";t.r(n),t.d(n,"frontMatter",(function(){return o})),t.d(n,"metadata",(function(){return s})),t.d(n,"toc",(function(){return i})),t.d(n,"default",(function(){return u}));var a=t(3),c=t(7),r=(t(0),t(457)),l=t(459),o={id:"constants-and-variables",title:"Constants & Variables"},s={unversionedId:"language-basics/constants-and-variables",id:"version-0.25.0/language-basics/constants-and-variables",isDocsHomePage:!1,title:"Constants & Variables",description:"The next building block after types are constants and variables.",source:"@site/versioned_docs/version-0.25.0/language-basics/variables-and-constants.md",slug:"/language-basics/constants-and-variables",permalink:"/docs/language-basics/constants-and-variables",version:"0.25.0",sidebar:"version-0.25.0/docs",previous:{title:"Types",permalink:"/docs/language-basics/types"},next:{title:"Math, Numbers & Tez",permalink:"/docs/language-basics/math-numbers-tez"}},i=[{value:"Constants",id:"constants",children:[]},{value:"Variables",id:"variables",children:[]}],b={toc:i};function u(e){var n=e.components,t=Object(c.a)(e,["components"]);return Object(r.b)("wrapper",Object(a.a)({},b,t,{components:n,mdxType:"MDXLayout"}),Object(r.b)("p",null,"The next building block after types are ",Object(r.b)("em",{parentName:"p"},"constants")," and ",Object(r.b)("em",{parentName:"p"},"variables"),"."),Object(r.b)("h2",{id:"constants"},"Constants"),Object(r.b)("p",null,"Constants are immutable by design, which means their values cannot be\nreassigned. Put in another way, they can be assigned once, at their\ndeclaration. When defining a constant you need to provide a ",Object(r.b)("inlineCode",{parentName:"p"},"name"),",\n",Object(r.b)("inlineCode",{parentName:"p"},"type")," and a ",Object(r.b)("inlineCode",{parentName:"p"},"value"),":"),Object(r.b)(l.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=a",group:"a"}),"const age : int = 25\n")),Object(r.b)("p",null,"You can evaluate the constant definition above using the following CLI\ncommand:"),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-shell"}),"ligo evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.ligo age\n# Outputs: 25\n"))),Object(r.b)(l.b,{syntax:"cameligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=a",group:"a"}),"let age : int = 25\n")),Object(r.b)("p",null,"You can evaluate the constant definition above using the following CLI\ncommand:"),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-shell"}),"ligo evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.mligo age\n# Outputs: 25\n"))),Object(r.b)(l.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=a",group:"a"}),"let age : int = 25;\n")),Object(r.b)("p",null,"You can evaluate the constant definition above using the following CLI\ncommand:"),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-shell"}),"ligo evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.religo age\n# Outputs: 25\n"))),Object(r.b)(l.b,{syntax:"jsligo",mdxType:"Syntax"},Object(r.b)("blockquote",null,Object(r.b)("p",{parentName:"blockquote"},"Constants in JsLIGO are enforced:")),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"skip",skip:!0}),"let x = (a: int): int => {\n  const age : int = 25;\n  age = 3; // gives an error\n};\n")),Object(r.b)("p",null,"Unlike the other syntaxes, JsLIGO doesn't allow variable names to be reused in the same block scope:"),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"skip",skip:!0}),"let x = (a: int): int => {\n  const age: int = 25;\n  const age: int = 3; // will give an error\n};\n")),Object(r.b)("p",null,"However, the following does work:"),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=d",group:"d"}),"let x = (a: int): int => {\n  const age: int = 25;\n  {\n     const age: int = 3; // does not give an error\n     return age;\n  }\n};\n")),Object(r.b)("blockquote",null,Object(r.b)("p",{parentName:"blockquote"},"This is not yet enforced in the experimental version of JsLIGO, but will be in the future.")),Object(r.b)("p",null,"You can evaluate the constant definition above using the following CLI\ncommand:"),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-shell"}),"ligo evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.jsligo age\n# Outputs: 25\n"))),Object(r.b)("h2",{id:"variables"},"Variables"),Object(r.b)(l.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(r.b)("p",null,"Variables, unlike constants, are ",Object(r.b)("em",{parentName:"p"},"mutable"),". They cannot be declared in\na ",Object(r.b)("em",{parentName:"p"},"global scope"),", but they can be declared and used within functions,\nor as function parameters."),Object(r.b)("blockquote",null,Object(r.b)("p",{parentName:"blockquote"},"\u26a0\ufe0f Please be wary that mutation only works within the function scope\nitself, values outside of the function scope will not be\naffected. In other words, when a function is called, its arguments\nare copied, ",Object(r.b)("em",{parentName:"p"},"as well as the environment"),". Any side-effect to that\nenvironment is therefore lost when the function returns.")),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-pascaligo",metastring:"group=b",group:"b"}),"// The following is invalid: use `const` for global values instead.\n// var four : int := 4\n\nfunction add (const a : int; const b : int) : int is\n  block {\n    var c : int := a + 2*b;\n    c := c - b\n  } with c\n")),Object(r.b)("blockquote",null,Object(r.b)("p",{parentName:"blockquote"},"\u26a0\ufe0f Notice the assignment operator ",Object(r.b)("inlineCode",{parentName:"p"},":=")," for ",Object(r.b)("inlineCode",{parentName:"p"},"var"),", instead of ",Object(r.b)("inlineCode",{parentName:"p"},"=")," for\nconstants.")),Object(r.b)("p",null,"You can run the ",Object(r.b)("inlineCode",{parentName:"p"},"add")," function defined above using the LIGO compiler\nlike this:"),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-shell"}),"ligo evaluate-call gitlab-pages/docs/language-basics/src/variables-and-constants/add.ligo add '(1,1)'\n# Outputs: 2\n"))),Object(r.b)(l.b,{syntax:"cameligo",mdxType:"Syntax"},Object(r.b)("p",null,"As expected in the pure subset of a functional language, CameLIGO only\nfeatures ",Object(r.b)("em",{parentName:"p"},"constant values"),': once they are declared, the value cannot\nbe changed (or "mutated").'),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-cameligo",metastring:"group=c",group:"c"}),"let add (a : int) (b : int) : int =\n  let c : int = a + b in c\n")),Object(r.b)("p",null,"You can run the ",Object(r.b)("inlineCode",{parentName:"p"},"add")," function defined above using the LIGO compiler\nlike this:"),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-shell"}),"ligo evaluate-call gitlab-pages/docs/language-basics/src/variables-and-constants/add.mligo add '(1,1)'\n# Outputs: 2\n"))),Object(r.b)(l.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(r.b)("p",null,"As expected in the pure subset of a functional language, ReasonLIGO\nonly features ",Object(r.b)("em",{parentName:"p"},"constant values"),': once they are declared, the value\ncannot be changed (or "mutated").'),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-reasonligo",metastring:"group=c",group:"c"}),"let add = ((a, b): (int, int)): int => {\n  let c : int = a + b;\n  c;\n};\n")),Object(r.b)("p",null,"You can run the ",Object(r.b)("inlineCode",{parentName:"p"},"add")," function defined above using the LIGO compiler\nlike this:"),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-shell"}),"ligo evaluate-call gitlab-pages/docs/language-basics/src/variables-and-constants/add.religo add '(1,1)'\n# Outputs: 2\n"))),Object(r.b)(l.b,{syntax:"jsligo",mdxType:"Syntax"},Object(r.b)("p",null,"Variables, unlike constants, are ",Object(r.b)("em",{parentName:"p"},"mutable"),". "),Object(r.b)("blockquote",null,Object(r.b)("p",{parentName:"blockquote"},"\u26a0\ufe0f Please be wary that mutation only works within the function scope\nitself, values outside of the function scope will not be\naffected. In other words, when a function is called, its arguments\nare copied, ",Object(r.b)("em",{parentName:"p"},"as well as the environment"),". Any side-effect to that\nenvironment is therefore lost when the function returns.")),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-jsligo",metastring:"group=b",group:"b"}),"let add = (a: int, b: int): int => {\n  let c = a;\n  c = c + b;\n  return c\n}\n")),Object(r.b)("p",null,"You can run the ",Object(r.b)("inlineCode",{parentName:"p"},"add")," function defined above using the LIGO compiler\nlike this:"),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-shell"}),"ligo evaluate-call gitlab-pages/docs/language-basics/src/variables-and-constants/add.jsligo add '(1,1)'\n# Outputs: 2\n"))))}u.isMDXComponent=!0},457:function(e,n,t){"use strict";t.d(n,"a",(function(){return u})),t.d(n,"b",(function(){return d}));var a=t(0),c=t.n(a);function r(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function l(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function o(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?l(Object(t),!0).forEach((function(n){r(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):l(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function s(e,n){if(null==e)return{};var t,a,c=function(e,n){if(null==e)return{};var t,a,c={},r=Object.keys(e);for(a=0;a<r.length;a++)t=r[a],n.indexOf(t)>=0||(c[t]=e[t]);return c}(e,n);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)t=r[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(c[t]=e[t])}return c}var i=c.a.createContext({}),b=function(e){var n=c.a.useContext(i),t=n;return e&&(t="function"==typeof e?e(n):o(o({},n),e)),t},u=function(e){var n=b(e.components);return c.a.createElement(i.Provider,{value:n},e.children)},p={inlineCode:"code",wrapper:function(e){var n=e.children;return c.a.createElement(c.a.Fragment,{},n)}},g=c.a.forwardRef((function(e,n){var t=e.components,a=e.mdxType,r=e.originalType,l=e.parentName,i=s(e,["components","mdxType","originalType","parentName"]),u=b(t),g=a,d=u["".concat(l,".").concat(g)]||u[g]||p[g]||r;return t?c.a.createElement(d,o(o({ref:n},i),{},{components:t})):c.a.createElement(d,o({ref:n},i))}));function d(e,n){var t=arguments,a=n&&n.mdxType;if("string"==typeof e||a){var r=t.length,l=new Array(r);l[0]=g;var o={};for(var s in n)hasOwnProperty.call(n,s)&&(o[s]=n[s]);o.originalType=e,o.mdxType="string"==typeof e?e:a,l[1]=o;for(var i=2;i<r;i++)l[i]=t[i];return c.a.createElement.apply(null,l)}return c.a.createElement.apply(null,t)}g.displayName="MDXCreateElement"},458:function(e,n,t){"use strict";var a=t(0),c=t.n(a).a.createContext("pascaligo");n.a=c},459:function(e,n,t){"use strict";var a=t(0),c=t.n(a),r=t(458);t.d(n,"a",(function(){return r.a})),n.b=function(e){return c.a.createElement(r.a.Consumer,null,(function(n){return n===e.syntax?e.children:c.a.createElement(c.a.Fragment,null)}))}}}]);