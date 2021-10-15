(window.webpackJsonp=window.webpackJsonp||[]).push([[277],{351:function(e,t,a){"use strict";a.r(t),a.d(t,"frontMatter",(function(){return l})),a.d(t,"metadata",(function(){return i})),a.d(t,"toc",(function(){return b})),a.d(t,"default",(function(){return p}));var n=a(3),s=a(7),r=(a(0),a(457)),o=a(459),c=a(464),l={id:"crypto-reference",title:"Crypto",description:"Cryptographic operations",hide_table_of_contents:!0},i={unversionedId:"reference/crypto-reference",id:"version-0.25.0/reference/crypto-reference",isDocsHomePage:!1,title:"Crypto",description:"Cryptographic operations",source:"@site/versioned_docs/version-0.25.0/reference/crypto.md",slug:"/reference/crypto-reference",permalink:"/docs/reference/crypto-reference",version:"0.25.0",sidebar:"version-0.25.0/docs",previous:{title:"Bytes",permalink:"/docs/reference/bytes-reference"},next:{title:"List",permalink:"/docs/reference/list-reference"}},b=[],y={toc:b};function p(e){var t=e.components,a=Object(s.a)(e,["components"]);return Object(r.b)("wrapper",Object(n.a)({},y,a,{components:t,mdxType:"MDXLayout"}),Object(r.b)(c.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function blake2b : bytes -> bytes"),Object(r.b)(c.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val blake2b : bytes -> bytes"),Object(r.b)(c.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let blake2b: bytes => bytes"),Object(r.b)(c.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let blake2b: (b: bytes) => bytes"),Object(r.b)("p",null,"Runs the ",Object(r.b)("a",Object(n.a)({parentName:"p"},{href:"https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE2"}),"blake2b hash algorithm"),"\nover the given ",Object(r.b)("inlineCode",{parentName:"p"},"bytes")," data and returns a ",Object(r.b)("inlineCode",{parentName:"p"},"bytes")," representing the hash."),Object(r.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo"}),"function hasherman_blake (const s: bytes) : bytes is Crypto.blake2b(s)\n"))),Object(r.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo"}),"let hasherman_blake (s: bytes) : bytes = Crypto.blake2b s\n"))),Object(r.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo"}),"let hasherman_blake = (s: bytes) => Crypto.blake2b(s);\n"))),Object(r.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo"}),"let hasherman_blake = (s: bytes):bytes => Crypto.blake2b(s);\n"))),Object(r.b)(c.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function sha256 : bytes -> bytes"),Object(r.b)(c.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val sha256 : bytes -> bytes"),Object(r.b)(c.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let sha256: bytes => bytes"),Object(r.b)(c.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let sha256: (b: bytes) => bytes"),Object(r.b)("p",null,"Runs the ",Object(r.b)("a",Object(n.a)({parentName:"p"},{href:"https://en.wikipedia.org/wiki/SHA-2"}),"sha256 hash algorithm")," over the given\n",Object(r.b)("inlineCode",{parentName:"p"},"bytes")," data and returns a ",Object(r.b)("inlineCode",{parentName:"p"},"bytes")," representing the hash."),Object(r.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo"}),"function hasherman (const s : bytes) : bytes is Crypto.sha256(s)\n"))),Object(r.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo"}),"let hasherman (s : bytes) : bytes =\n   Crypto.sha256 s\n"))),Object(r.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo"}),"let hasherman = (s: bytes): bytes => Crypto.sha256(s);\n"))),Object(r.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo"}),"let hasherman = (s: bytes): bytes => Crypto.sha256(s);\n"))),Object(r.b)(c.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function sha512 : bytes -> bytes"),Object(r.b)(c.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val sha512 : bytes -> bytes"),Object(r.b)(c.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let sha512: bytes => bytes"),Object(r.b)(c.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let sha512: (b: bytes) => bytes"),Object(r.b)("p",null,"Runs the ",Object(r.b)("a",Object(n.a)({parentName:"p"},{href:"https://en.wikipedia.org/wiki/SHA-2"}),"sha512 hash algorithm")," over the given\n",Object(r.b)("inlineCode",{parentName:"p"},"bytes")," data and returns a ",Object(r.b)("inlineCode",{parentName:"p"},"bytes")," representing the hash."),Object(r.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo"}),"function hasherman512 (const s: bytes) : bytes is Crypto.sha512(s)\n"))),Object(r.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo"}),"let hasherman512 (s: bytes) : bytes = Crypto.sha512 s\n"))),Object(r.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo"}),"let hasherman512 = (s: bytes) => Crypto.sha512(s);\n"))),Object(r.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo"}),"let hasherman512 = (s: bytes): bytes => Crypto.sha512(s);\n"))),Object(r.b)(c.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function sha3 : bytes -> bytes"),Object(r.b)(c.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val sha3 : bytes -> bytes"),Object(r.b)(c.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let sha3: bytes => bytes"),Object(r.b)(c.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let sha3: (b: bytes) => bytes"),Object(r.b)("p",null,"Runs the ",Object(r.b)("a",Object(n.a)({parentName:"p"},{href:"https://en.wikipedia.org/wiki/SHA-3"}),"sha3 hash algorithm")," over the given\n",Object(r.b)("inlineCode",{parentName:"p"},"bytes")," data and returns a ",Object(r.b)("inlineCode",{parentName:"p"},"bytes")," representing the hash."),Object(r.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo"}),"function hasherman3 (const s: bytes) : bytes is Crypto.sha3(s)\n"))),Object(r.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo"}),"let hasherman3 (s: bytes) : bytes = Crypto.sha3 s\n"))),Object(r.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo"}),"let hasherman3 = (s: bytes) => Crypto.sha3(s);\n"))),Object(r.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo"}),"let hasherman3 = (s: bytes): bytes => Crypto.sha3(s);\n"))),Object(r.b)(c.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function keccak : bytes -> bytes"),Object(r.b)(c.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val keccak : bytes -> bytes"),Object(r.b)(c.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let keccak: bytes => bytes"),Object(r.b)(c.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let keccak: (b: bytes) => bytes"),Object(r.b)("p",null,"Runs the ",Object(r.b)("a",Object(n.a)({parentName:"p"},{href:"https://en.wikipedia.org/wiki/keccak"}),"keccak")," over the given\n",Object(r.b)("inlineCode",{parentName:"p"},"bytes")," data and returns a ",Object(r.b)("inlineCode",{parentName:"p"},"bytes")," representing the hash."),Object(r.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo"}),"function hasherman_keccak (const s: bytes) : bytes is Crypto.keccak(s)\n"))),Object(r.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo"}),"let hasherman_keccak (s: bytes) : bytes = Crypto.keccak s\n"))),Object(r.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo"}),"let hasherman_keccak = (s: bytes) => Crypto.keccak(s);\n"))),Object(r.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo"}),"let hasherman_keccak = (s: bytes): bytes => Crypto.keccak(s);\n"))),Object(r.b)(c.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function hash_key : key -> key_hash"),Object(r.b)(c.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val hash_key : key -> key_hash"),Object(r.b)(c.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let hash_key: key => key_hash"),Object(r.b)(c.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let hash_key: (k: key) => key_hash"),Object(r.b)("p",null,"Hashes a key for easy comparison and storage."),Object(r.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo"}),"function check_hash_key (const kh1 : key_hash; const k2 : key) : bool * key_hash is block {\n  var ret : bool := False ;\n  var kh2 : key_hash := Crypto.hash_key(k2) ;\n  if kh1 = kh2 then ret := True else skip; \n} with (ret, kh2)\n"))),Object(r.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo"}),"let check_hash_key (kh1, k2: key_hash * key) : bool * key_hash =\n  let kh2 : key_hash = Crypto.hash_key k2 in\n  if kh1 = kh2\n  then (true, kh2)\n  else (false, kh2)\n"))),Object(r.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo"}),"let check_hash_key = ((kh1, k2): (key_hash, key)) : (bool, key_hash) => {\n  let kh2 : key_hash = Crypto.hash_key(k2);\n  if (kh1 == kh2) {\n    (true, kh2);\n  }\n  else {\n    (false, kh2);\n  }\n};\n"))),Object(r.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo"}),"let check_hash_key = ([kh1, k2]: [key_hash, key]) : [bool, key_hash] => {\n  let kh2 : key_hash = Crypto.hash_key(k2);\n  if (kh1 == kh2) {\n    return [true, kh2];\n  }\n  else {\n    return [false, kh2];\n  };\n};\n"))),Object(r.b)(c.a,{syntax:"pascaligo",mdxType:"SyntaxTitle"},"function check : key -> signature -> bytes -> bool"),Object(r.b)(c.a,{syntax:"cameligo",mdxType:"SyntaxTitle"},"val check : key -> signature -> bytes -> bool"),Object(r.b)(c.a,{syntax:"reasonligo",mdxType:"SyntaxTitle"},"let check: (key, signature, bytes) => bool"),Object(r.b)(c.a,{syntax:"jsligo",mdxType:"SyntaxTitle"},"let check: (k: key, s: signature, b: bytes) => bool"),Object(r.b)("p",null,"Check that a message has been signed by a particular key."),Object(r.b)("blockquote",null,Object(r.b)("p",{parentName:"blockquote"},"\u26a0\ufe0f There is no way to ",Object(r.b)("em",{parentName:"p"},"generate")," a signed message in LIGO. This is because that would require storing a private key on chain, at which point it isn't very private anymore.")),Object(r.b)(o.b,{syntax:"pascaligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-pascaligo"}),"function check_signature\n    (const pk: key;\n     const signed: signature;\n     const msg: bytes) : bool\n  is Crypto.check(pk, signed, msg)\n"))),Object(r.b)(o.b,{syntax:"cameligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-cameligo"}),"let check_signature (pk, signed, msg: key * signature * bytes) : bool =\n  Crypto.check pk signed msg\n"))),Object(r.b)(o.b,{syntax:"reasonligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-reasonligo"}),"let check_signature = ((pk, signed, msg): (key, signature, bytes)) : bool => {\n  Crypto.check(pk, signed, msg);\n};\n"))),Object(r.b)(o.b,{syntax:"jsligo",mdxType:"Syntax"},Object(r.b)("pre",null,Object(r.b)("code",Object(n.a)({parentName:"pre"},{className:"language-jsligo"}),"let check_signature = ([pk, signed, msg]: [key, signature, bytes]) : bool => {\n  return Crypto.check(pk, signed, msg);\n};\n"))))}p.isMDXComponent=!0},457:function(e,t,a){"use strict";a.d(t,"a",(function(){return y})),a.d(t,"b",(function(){return g}));var n=a(0),s=a.n(n);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function c(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function l(e,t){if(null==e)return{};var a,n,s=function(e,t){if(null==e)return{};var a,n,s={},r=Object.keys(e);for(n=0;n<r.length;n++)a=r[n],t.indexOf(a)>=0||(s[a]=e[a]);return s}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(n=0;n<r.length;n++)a=r[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(s[a]=e[a])}return s}var i=s.a.createContext({}),b=function(e){var t=s.a.useContext(i),a=t;return e&&(a="function"==typeof e?e(t):c(c({},t),e)),a},y=function(e){var t=b(e.components);return s.a.createElement(i.Provider,{value:t},e.children)},p={inlineCode:"code",wrapper:function(e){var t=e.children;return s.a.createElement(s.a.Fragment,{},t)}},u=s.a.forwardRef((function(e,t){var a=e.components,n=e.mdxType,r=e.originalType,o=e.parentName,i=l(e,["components","mdxType","originalType","parentName"]),y=b(a),u=n,g=y["".concat(o,".").concat(u)]||y[u]||p[u]||r;return a?s.a.createElement(g,c(c({ref:t},i),{},{components:a})):s.a.createElement(g,c({ref:t},i))}));function g(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var r=a.length,o=new Array(r);o[0]=u;var c={};for(var l in t)hasOwnProperty.call(t,l)&&(c[l]=t[l]);c.originalType=e,c.mdxType="string"==typeof e?e:n,o[1]=c;for(var i=2;i<r;i++)o[i]=a[i];return s.a.createElement.apply(null,o)}return s.a.createElement.apply(null,a)}u.displayName="MDXCreateElement"},458:function(e,t,a){"use strict";var n=a(0),s=a.n(n).a.createContext("pascaligo");t.a=s},459:function(e,t,a){"use strict";var n=a(0),s=a.n(n),r=a(458);a.d(t,"a",(function(){return r.a})),t.b=function(e){return s.a.createElement(r.a.Consumer,null,(function(t){return t===e.syntax?e.children:s.a.createElement(s.a.Fragment,null)}))}},460:function(e,t,a){"use strict";a.r(t),a.d(t,"Prism",(function(){return n.a})),a.d(t,"defaultProps",(function(){return o}));var n=a(22),s={plain:{backgroundColor:"#2a2734",color:"#9a86fd"},styles:[{types:["comment","prolog","doctype","cdata","punctuation"],style:{color:"#6c6783"}},{types:["namespace"],style:{opacity:.7}},{types:["tag","operator","number"],style:{color:"#e09142"}},{types:["property","function"],style:{color:"#9a86fd"}},{types:["tag-id","selector","atrule-id"],style:{color:"#eeebff"}},{types:["attr-name"],style:{color:"#c4b9fe"}},{types:["boolean","string","entity","url","attr-value","keyword","control","directive","unit","statement","regex","at-rule","placeholder","variable"],style:{color:"#ffcc99"}},{types:["deleted"],style:{textDecorationLine:"line-through"}},{types:["inserted"],style:{textDecorationLine:"underline"}},{types:["italic"],style:{fontStyle:"italic"}},{types:["important","bold"],style:{fontWeight:"bold"}},{types:["important"],style:{color:"#c4b9fe"}}]},r=a(0),o={Prism:n.a,theme:s};function c(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function l(){return(l=Object.assign||function(e){for(var t=1;t<arguments.length;t++){var a=arguments[t];for(var n in a)Object.prototype.hasOwnProperty.call(a,n)&&(e[n]=a[n])}return e}).apply(this,arguments)}var i=/\r\n|\r|\n/,b=function(e){0===e.length?e.push({types:["plain"],content:"",empty:!0}):1===e.length&&""===e[0].content&&(e[0].empty=!0)},y=function(e,t){var a=e.length;return a>0&&e[a-1]===t?e:e.concat(t)},p=function(e,t){var a=e.plain,n=Object.create(null),s=e.styles.reduce((function(e,a){var n=a.languages,s=a.style;return n&&!n.includes(t)||a.types.forEach((function(t){var a=l({},e[t],s);e[t]=a})),e}),n);return s.root=a,s.plain=l({},a,{backgroundColor:null}),s};function u(e,t){var a={};for(var n in e)Object.prototype.hasOwnProperty.call(e,n)&&-1===t.indexOf(n)&&(a[n]=e[n]);return a}var g=function(e){function t(){for(var t=this,a=[],n=arguments.length;n--;)a[n]=arguments[n];e.apply(this,a),c(this,"getThemeDict",(function(e){if(void 0!==t.themeDict&&e.theme===t.prevTheme&&e.language===t.prevLanguage)return t.themeDict;t.prevTheme=e.theme,t.prevLanguage=e.language;var a=e.theme?p(e.theme,e.language):void 0;return t.themeDict=a})),c(this,"getLineProps",(function(e){var a=e.key,n=e.className,s=e.style,r=l({},u(e,["key","className","style","line"]),{className:"token-line",style:void 0,key:void 0}),o=t.getThemeDict(t.props);return void 0!==o&&(r.style=o.plain),void 0!==s&&(r.style=void 0!==r.style?l({},r.style,s):s),void 0!==a&&(r.key=a),n&&(r.className+=" "+n),r})),c(this,"getStyleForToken",(function(e){var a=e.types,n=e.empty,s=a.length,r=t.getThemeDict(t.props);if(void 0!==r){if(1===s&&"plain"===a[0])return n?{display:"inline-block"}:void 0;if(1===s&&!n)return r[a[0]];var o=n?{display:"inline-block"}:{},c=a.map((function(e){return r[e]}));return Object.assign.apply(Object,[o].concat(c))}})),c(this,"getTokenProps",(function(e){var a=e.key,n=e.className,s=e.style,r=e.token,o=l({},u(e,["key","className","style","token"]),{className:"token "+r.types.join(" "),children:r.content,style:t.getStyleForToken(r),key:void 0});return void 0!==s&&(o.style=void 0!==o.style?l({},o.style,s):s),void 0!==a&&(o.key=a),n&&(o.className+=" "+n),o}))}return e&&(t.__proto__=e),t.prototype=Object.create(e&&e.prototype),t.prototype.constructor=t,t.prototype.render=function(){var e=this.props,t=e.Prism,a=e.language,n=e.code,s=e.children,r=this.getThemeDict(this.props),o=t.languages[a];return s({tokens:function(e){for(var t=[[]],a=[e],n=[0],s=[e.length],r=0,o=0,c=[],l=[c];o>-1;){for(;(r=n[o]++)<s[o];){var p=void 0,u=t[o],g=a[o][r];if("string"==typeof g?(u=o>0?u:["plain"],p=g):(u=y(u,g.type),g.alias&&(u=y(u,g.alias)),p=g.content),"string"==typeof p){var m=p.split(i),h=m.length;c.push({types:u,content:m[0]});for(var d=1;d<h;d++)b(c),l.push(c=[]),c.push({types:u,content:m[d]})}else o++,t.push(u),a.push(p),n.push(0),s.push(p.length)}o--,t.pop(),a.pop(),n.pop(),s.pop()}return b(c),l}(void 0!==o?t.tokenize(n,o,a):[n]),className:"prism-code language-"+a,style:void 0!==r?r.root:{},getLineProps:this.getLineProps,getTokenProps:this.getTokenProps})},t}(r.Component);t.default=g},461:function(e,t,a){"use strict";var n=a(0),s=a(463);t.a=function(){var e=Object(n.useContext)(s.a);if(null==e)throw new Error("`useThemeContext` is used outside of `Layout` Component. See https://v2.docusaurus.io/docs/theme-classic#usethemecontext.");return e}},462:function(e,t,a){"use strict";t.a={plain:{color:"#bfc7d5",backgroundColor:"#292d3e"},styles:[{types:["comment"],style:{color:"rgb(105, 112, 152)",fontStyle:"italic"}},{types:["string","inserted"],style:{color:"rgb(195, 232, 141)"}},{types:["number"],style:{color:"rgb(247, 140, 108)"}},{types:["builtin","char","constant","function"],style:{color:"rgb(130, 170, 255)"}},{types:["punctuation","selector"],style:{color:"rgb(199, 146, 234)"}},{types:["variable"],style:{color:"rgb(191, 199, 213)"}},{types:["class-name","attr-name"],style:{color:"rgb(255, 203, 107)"}},{types:["tag","deleted"],style:{color:"rgb(255, 85, 114)"}},{types:["operator"],style:{color:"rgb(137, 221, 255)"}},{types:["boolean"],style:{color:"rgb(255, 88, 116)"}},{types:["keyword"],style:{fontStyle:"italic"}},{types:["doctype"],style:{color:"rgb(199, 146, 234)",fontStyle:"italic"}},{types:["namespace"],style:{color:"rgb(178, 204, 214)"}},{types:["url"],style:{color:"rgb(221, 221, 221)"}}]}},463:function(e,t,a){"use strict";var n=a(0),s=a.n(n).a.createContext(void 0);t.a=s},464:function(e,t,a){"use strict";var n=a(0),s=a.n(n),r=a(460),o=a(23),c=a(461),l=a(459),i=a(462);function b(){return(b=Object.assign||function(e){for(var t=1;t<arguments.length;t++){var a=arguments[t];for(var n in a)Object.prototype.hasOwnProperty.call(a,n)&&(e[n]=a[n])}return e}).apply(this,arguments)}var y=a(460).Prism;y.languages=Object.assign({},y.languages,{pascaligo:{comment:[/\(\*[\s\S]+?\*\)/,/\/\/.*/],string:{pattern:/(?:'(?:''|[^'\r\n])*'|#[&$%]?[a-f\d]+)+|\^[a-z]/i,greedy:!0},keyword:[{pattern:/(^|[^&])\b(?:absolute|array|asm|begin|case|const|constructor|destructor|do|downto|else|end|file|for|function|goto|if|implementation|inherited|inline|interface|label|nil|object|of|operator|packed|procedure|program|record|reintroduce|repeat|self|set|string|then|to|type|unit|until|uses|var|while|with)\b/i,lookbehind:!0},{pattern:/(^|[^&])\b(?:dispose|exit|false|new|true)\b/i,lookbehind:!0},{pattern:/(^|[^&])\b(?:class|dispinterface|except|exports|finalization|finally|initialization|inline|library|on|out|packed|property|raise|resourcestring|threadvar|try)\b/i,lookbehind:!0},{pattern:/(^|[^&])\b(?:absolute|abstract|alias|assembler|bitpacked|break|cdecl|continue|cppdecl|cvar|default|deprecated|dynamic|enumerator|experimental|export|external|far|far16|forward|generic|helper|implements|index|interrupt|iochecks|local|message|name|near|nodefault|noreturn|nostackframe|oldfpccall|otherwise|overload|override|pascal|platform|private|protected|public|published|read|register|reintroduce|result|safecall|saveregisters|softfloat|specialize|static|stdcall|stored|strict|unaligned|unimplemented|varargs|virtual|write)\b/i,lookbehind:!0}],number:[/(?:[&%]\d+|\$[a-f\d]+)/i,/\b\d+(?:\.\d+)?(?:e[+-]?\d+)?/i],operator:[/\.\.|\*\*|:=|<[<=>]?|>[>=]?|[+\-*\/]=?|[@^=]/i,{pattern:/(^|[^&])\b(?:and|as|div|exclude|in|include|is|mod|not|or|shl|shr|xor)\b/,lookbehind:!0}],punctuation:/\(\.|\.\)|[()\[\]:;,.]/},reasonligo:Object.assign({},y.languages.reason,{comment:[/(^|[^\\])\/\*[\s\S]*?\*\//,/\(\*[\s\S]*?\*\)/,/\/\/.*/]}),cameligo:Object.assign({},y.languages.ocaml,{comment:[/(^|[^\\])\/\*[\s\S]*?\*\//,/\(\*[\s\S]*?\*\)/,/\/\/.*/]}),jsligo:y.languages.typescript}),t.a=function(e){var t=Object(o.default)().siteConfig.themeConfig.prism,a=void 0===t?{}:t,y=Object(c.a)().isDarkTheme,p=a.theme||i.a,u=a.darkTheme||p,g=y?u:p,m=Object(n.useState)(!1),h=m[0],d=m[1];return Object(n.useEffect)((function(){d(!0)}),[]),s.a.createElement(l.a.Consumer,null,(function(t){return t===e.syntax?s.a.createElement(r.default,b({},r.defaultProps,{key:h,language:e.syntax,code:e.children,theme:g}),(function(e){var t=e.className,a=(e.style,e.tokens),n=e.getLineProps,r=e.getTokenProps;return s.a.createElement("pre",{className:t,style:{backgroundColor:"var(--ifm-background-color)",fontSize:"1.1rem",fontWeight:"bold",padding:0,whiteSpace:"break-spaces",marginTop:"3rem"}},a.map((function(e,t){return s.a.createElement("div",n({line:e,key:t}),e.map((function(e,t){return s.a.createElement("span",r({token:e,key:t}))})))})))})):s.a.createElement("div",null)}))}}}]);