(window.webpackJsonp=window.webpackJsonp||[]).push([[360],{433:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return c})),n.d(t,"metadata",(function(){return b})),n.d(t,"toc",(function(){return p})),n.d(t,"default",(function(){return i}));var a=n(3),r=n(7),o=(n(0),n(457)),c={},b={unversionedId:"manpages/ligo",id:"version-0.25.0/manpages/ligo",isDocsHomePage:!1,title:"ligo",description:"NAME",source:"@site/versioned_docs/version-0.25.0/manpages/ligo.md",slug:"/manpages/ligo",permalink:"/docs/manpages/ligo",version:"0.25.0",sidebar:"version-0.25.0/docs",previous:{title:"Cheat Sheet",permalink:"/docs/api/cheat-sheet"},next:{title:"changelog",permalink:"/docs/manpages/changelog"}},p=[{value:"NAME",id:"name",children:[]},{value:"SYNOPSIS",id:"synopsis",children:[]},{value:"MORE HELP",id:"more-help",children:[]},{value:"DOCUMENTATION",id:"documentation",children:[]},{value:"ASK A QUESTION",id:"ask-a-question",children:[]},{value:"OPEN AN ISSUE",id:"open-an-issue",children:[]},{value:"COMMANDS",id:"commands",children:[]},{value:"OPTIONS",id:"options",children:[]}],l={toc:p};function i(e){var t=e.components,n=Object(r.a)(e,["components"]);return Object(o.b)("wrapper",Object(a.a)({},l,n,{components:t,mdxType:"MDXLayout"}),Object(o.b)("h3",{id:"name"},"NAME"),Object(o.b)("p",null,"ligo"),Object(o.b)("h3",{id:"synopsis"},"SYNOPSIS"),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"ligo")," ",Object(o.b)("em",{parentName:"p"},"COMMAND")," ",".",".."),Object(o.b)("h3",{id:"more-help"},"MORE HELP"),Object(o.b)("p",null,"Use ","`",Object(o.b)("strong",{parentName:"p"},"ligo")," ",Object(o.b)("em",{parentName:"p"},"COMMAND")," ","-","-help","`"," for help on a single command."),Object(o.b)("h3",{id:"documentation"},"DOCUMENTATION"),Object(o.b)("p",null,Object(o.b)("a",Object(a.a)({parentName:"p"},{href:"https://ligolang.org/docs/intro/introduction"}),"https://ligolang.org/docs/intro/introduction")),Object(o.b)("h3",{id:"ask-a-question"},"ASK A QUESTION"),Object(o.b)("p",null,Object(o.b)("a",Object(a.a)({parentName:"p"},{href:"https://discord.gg/9rhYaEt"}),"https://discord.gg/9rhYaEt")),Object(o.b)("h3",{id:"open-an-issue"},"OPEN AN ISSUE"),Object(o.b)("p",null,Object(o.b)("a",Object(a.a)({parentName:"p"},{href:"https://gitlab.com/ligolang/ligo/issues/new"}),"https://gitlab.com/ligolang/ligo/issues/new")),Object(o.b)("h3",{id:"commands"},"COMMANDS"),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"changelog")),Object(o.b)("p",null,":   Dump the LIGO changelog to stdout."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"compile-contract")),Object(o.b)("p",null,":   Subcommand: Compile a contract."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"compile-expression")),Object(o.b)("p",null,":   Subcommand: Compile to a Michelson value."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"compile-parameter")),Object(o.b)("p",null,":   Subcommand: Compile parameters to a Michelson expression."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"compile-storage")),Object(o.b)("p",null,":   Subcommand: Compile an initial storage in LIGO syntax to a Michelson\nexpression."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"dry-run")),Object(o.b)("p",null,":   Subcommand: Run a smart-contract with the given storage and input."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"evaluate-call")),Object(o.b)("p",null,":   Subcommand: Run a function with the given parameter."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"evaluate-expr")),Object(o.b)("p",null,":   Subcommand: Evaluate a given definition."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"evaluate-value")),Object(o.b)("p",null,":   Deprecated, renamed to evaluate-expr. Use evaluate-expr instead.\nSubcommand: Evaluate a given definition."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"get-scope")),Object(o.b)("p",null,":   Subcommand: Return the JSON encoded environment for a given file."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"interpret")),Object(o.b)("p",null,":   Subcommand: Interpret the expression in the context initialized by\nthe provided source file."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"list-declarations")),Object(o.b)("p",null,":   Subcommand: List all the top-level declarations."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"measure-contract")),Object(o.b)("p",null,":   Subcommand: Measure a contract","`","s compiled size in bytes."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"preprocess")),Object(o.b)("p",null,":   Subcommand: Preprocess the source file. Warning: Intended for\ndevelopment of LIGO and can break at any time."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"pretty-print")),Object(o.b)("p",null,":   Subcommand: Pretty-print the source file."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"print-ast")),Object(o.b)("p",null,":   Subcommand: Print the AST. Warning: Intended for development of LIGO\nand can break at any time."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"print-ast-combined")),Object(o.b)("p",null,":   Subcommand: Print the contract after combination with the build\nsystem. Warning: Intended for development of LIGO and can break at\nany time."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"print-ast-core")),Object(o.b)("p",null,":   Subcommand: Print the AST. Warning: Intended for development of LIGO\nand can break at any time."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"print-ast-sugar")),Object(o.b)("p",null,":   Subcommand: Print the AST. Warning: Intended for development of LIGO\nand can break at any time."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"print-ast-typed")),Object(o.b)("p",null,":   Subcommand: Print the typed AST. Warning: Intended for development\nof LIGO and can break at any time."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"print-cst")),Object(o.b)("p",null,":   Subcommand: Print the CST. Warning: Intended for development of LIGO\nand can break at any time."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"print-graph")),Object(o.b)("p",null,":   Subcommand: Print the dependency graph. Warning: Intended for\ndevelopment of LIGO and can break at any time."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"print-mini-c")),Object(o.b)("p",null,":   Subcommand: Print Mini-C. Warning: Intended for development of LIGO\nand can break at any time."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"repl")),Object(o.b)("p",null,":   Subcommand: REPL"),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"run-function")),Object(o.b)("p",null,":   Deprecated, renamed to evaluate-call. Use evaluate-call instead.\nSubcommand: Run a function with the given parameter."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"test")),Object(o.b)("p",null,":   Subcommand: Test a contract with the LIGO test framework (BETA)."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"transpile-contract")),Object(o.b)("p",null,":   Subcommand: Transpile a contract to another syntax (BETA)."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"transpile-expression")),Object(o.b)("p",null,":   Subcommand: Transpile an expression to another syntax (BETA)."),Object(o.b)("h3",{id:"options"},"OPTIONS"),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"-","-help"),"[","=",Object(o.b)("em",{parentName:"p"},"FMT"),"]"," (default=auto)"),Object(o.b)("p",null,":   Show this help in format ",Object(o.b)("em",{parentName:"p"},"FMT"),". The value ",Object(o.b)("em",{parentName:"p"},"FMT")," must be one of\n","`","auto","`",", ","`","pager","`",", ","`","groff","`"," or ","`","plain","`",". With ","`","auto","`",", the\nformat is ","`","pager","`"," or ","`","plain","`"," whenever the ",Object(o.b)("strong",{parentName:"p"},"TERM")," env var is\n","`","dumb","`"," or undefined."),Object(o.b)("p",null,Object(o.b)("strong",{parentName:"p"},"-","-version")),Object(o.b)("p",null,":   Show version information."))}i.isMDXComponent=!0},457:function(e,t,n){"use strict";n.d(t,"a",(function(){return u})),n.d(t,"b",(function(){return O}));var a=n(0),r=n.n(a);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function c(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function b(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?c(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):c(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function p(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var l=r.a.createContext({}),i=function(e){var t=r.a.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):b(b({},t),e)),n},u=function(e){var t=i(e.components);return r.a.createElement(l.Provider,{value:t},e.children)},m={inlineCode:"code",wrapper:function(e){var t=e.children;return r.a.createElement(r.a.Fragment,{},t)}},s=r.a.forwardRef((function(e,t){var n=e.components,a=e.mdxType,o=e.originalType,c=e.parentName,l=p(e,["components","mdxType","originalType","parentName"]),u=i(n),s=a,O=u["".concat(c,".").concat(s)]||u[s]||m[s]||o;return n?r.a.createElement(O,b(b({ref:t},l),{},{components:n})):r.a.createElement(O,b({ref:t},l))}));function O(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=n.length,c=new Array(o);c[0]=s;var b={};for(var p in t)hasOwnProperty.call(t,p)&&(b[p]=t[p]);b.originalType=e,b.mdxType="string"==typeof e?e:a,c[1]=b;for(var l=2;l<o;l++)c[l]=n[l];return r.a.createElement.apply(null,c)}return r.a.createElement.apply(null,n)}s.displayName="MDXCreateElement"}}]);