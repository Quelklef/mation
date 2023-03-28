const fs = require('fs');
const plib = require('path');

function main() {
  const emit = (name, f) => {
    fs.writeFileSync(
      plib.resolve(__dirname, name),
      [...f()].join('\n') + '\n'
    );
  }

  emit('Tags.purs', tags)
  emit('Attributes.purs', attributes)
  emit('Events.purs', events)
  emit('Styles.purs', styles)
  emit('PseudoClasses.purs', pseudoClasses);
}


// Convert a string to a similar purescript identifier
function toIdent(str) {
  let ident = str;

  // kebab-case -> camelCase
  ident = (
    ident
    .split('-')
    .map((part, idx) => idx === 0 ? part : part.slice(0, 1).toUpperCase() + part.slice(1))
    .join('')
  );

  // Ensure not a Purescript identifier
  const psKeywords = new Set(['class', 'data', 'type', 'where']);
  if (psKeywords.has(ident))
    ident += '_';

  return ident;
}

const preamble = what => (
`-- | This module contains generated code relating to ${what}.
-- |
-- | Names are converted from their canonical \`kebab-case\` form into \`camelCase\`, which is idiomatic for Purescript. For example, \`align-content\` becomes \`alignContent\`. Purescript-reserved words are suffixed by an understore, so \`type\` becomes \`type_\`.`
);

function * tags() {
  yield preamble('HTML tags');
  yield '';
  yield 'module Mation.Gen.Tags where';
  yield '';
  yield 'import Mation.Core.Html (Html)';
  yield 'import Mation.Core.Prop (Prop, mkElement)';
  yield '';
  yield '';

  // FIXME. Could be nice to expose for teach tag T a function `T` and
  //   a function `T'` which takes an extra string as parameter and exposes
  //   as a dom attribute such as data-mation-name. This would allow for the
  //   user to label the rendered DOM nodes which would help both with:
  //   - readability, since names would act as mini-comments in the code; and
  //   - debugging, because the DOM tree would have semantics labels

  const { tags } = require('./data/tags.js');
  for (const tag of tags) {
    const ident = toIdent(tag.name);

    yield `-- | [HTML <${tag.name}> tag](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/${encodeURIComponent(tag.name)}). This is generated code.`;
    if (!tag.isVoid) {
      yield `${ident} :: forall m s. Array (Prop m s) -> Array (Html m s) -> Html m s`;
      yield `${ident} props children = mkElement "${tag.name}" props children`
    } else {
      yield `${ident} :: forall m s. Array (Prop m s) -> Html m s`;
      yield `${ident} props = mkElement "${tag.name}" props []`
    }
    yield '';
  }
}

function * attributes() {
  yield preamble('HTML attributes');
  yield '';
  yield 'module Mation.Gen.Attributes where';
  yield '';
  yield 'import Mation.Core.Prop (Prop, mkPair, mkNoop)';
  yield '';
  yield '';

  const { attributes } = require('./data/attributes.js');
  for (const attr of attributes) {
    const ident = toIdent(attr.name);

    yield `-- | [HTML ${attr.name} attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/${encodeURIComponent(attr.name)}). This is generated code.`;
    if (attr.isBoolean) {
      yield `${ident} :: forall m s. Boolean -> Prop m s`;
      yield `${ident} bool = if bool then mkPair "${attr.name}" "${attr.name}" else mkNoop`;
    } else {
      yield `${ident} :: forall m s. String -> Prop m s`;
      yield `${ident} = mkPair "${attr.name}"`;
    }
    yield '';
  }
}

function * events() {
  yield preamble('HTML events');
  yield '';
  yield 'module Mation.Gen.Events where';
  yield '';
  yield 'import Mation.Core.Mation (Mation)';
  yield 'import Mation.Core.Dom (DomEvent)';
  yield 'import Mation.Core.Prop (Prop, mkListener)';
  yield '';
  yield '';

  const { events } = require('./data/events.js');
  for (const event of events) {
    const onName = 'on' + capitalize(toIdent(event.name));

    yield `-- | [HTML ${event.name} event](https://developer.mozilla.org/en-US/docs/Web/API/Element/${encodeURIComponent(event.name)}_event). This is generated code.`;
    yield `${onName} :: forall m s. (DomEvent -> Mation m s) -> Prop m s`;
    yield `${onName} = mkListener "${event.name}"`;
    yield '';
  }
}

function * styles() {
  yield preamble('CSS styles');
  yield '';
  yield 'module Mation.Gen.Styles where';
  yield '';
  yield 'import Mation.Core.Style (Style, mkPair)';
  yield '';
  yield '';

  const { styles } = require('./data/styles.js');
  for (const style of styles) {
    const ident = toIdent(style.name);

    yield `-- | [CSS ${style.name} property](https://developer.mozilla.org/en-US/docs/Web/CSS/${encodeURIComponent(style.name)}). This is generated code.`;
    yield `${ident} :: String -> Style`;
    yield `${ident} = mkPair "${style.name}"`
    yield '';
  }
}

function * pseudoClasses() {
  yield preamble('CSS pseudo-classes');
  yield '';
  yield 'module Mation.Gen.PseudoClasses where';
  yield '';
  yield 'import Mation.Core.Prelude';
  yield '';
  yield 'import Mation.Styles (Scope (..))';
  yield 'import Mation.Core.Util.Weave as W';
  yield '';
  yield '';

  const { pseudoClasses } = require('./data/pseudo-classes.js');
  for (const pcls of pseudoClasses) {
    const ident = toIdent(pcls.name);

    const args = range(pcls.arity).map(n => `x${n+1} `).join('');
    const argsCall = pcls.arity === 0 ? '' : ('(' + range(pcls.arity).map(n => `" <> x${n+1} <> "`).join(', ') + ')');
    const argTypes = range(pcls.arity).map(_ => `String -> `).join('');

    yield `-- | [CSS :${pcls.name} pseudo-class](https://developer.mozilla.org/en-US/docs/Web/CSS/:${pcls.name}). This is generated code.`;
    yield `${ident} :: ${argTypes}Scope`
    yield `${ident} ${args}= ScopeAlts [ { selector: W.Weave [ W.Hole, W.Elem $ ":${pcls.name}${argsCall}" ], block: W.noop } ]`;
    yield '';
  }
}


function capitalize(str) {
  return str.slice(0, 1).toUpperCase() + str.slice(1);
}

function range(n) {
  return new Array(n).fill(null).map((_, i) => i);
}

main();
