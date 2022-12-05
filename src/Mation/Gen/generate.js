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
  const psKeywords = new Set(['class', 'data', 'type']);
  if (psKeywords.has(ident))
    ident += '_';

  return ident;
}

function * tags() {
  yield 'module Mation.Gen.Tags where';
  yield '';
  yield 'import Mation.Core.Html (Prop, Html, mkElement)';
  yield '';
  yield '';

  const { tags } = require('./data/tags.js');
  for (const tag of tags) {
    const ident = toIdent(tag.name);

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
  yield 'module Mation.Gen.Attributes where';
  yield '';
  yield 'import Prelude';
  yield 'import Mation.Core.Html (Prop, mkPair, mkNoop)';
  yield '';
  yield '';

  const { attributes } = require('./data/attributes.js');
  for (const attr of attributes) {
    const ident = toIdent(attr.name);

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
  yield 'module Mation.Gen.Events where';
  yield '';
  yield 'import Prelude';
  yield 'import Mation.Core.Mation (Mation)';
  yield 'import Mation.Core.Dom (DomEvent)';
  yield 'import Mation.Core.Html (Prop, mkListener)';
  yield '';
  yield '';

  const { events } = require('./data/events.js');
  for (const event of events) {
    const onName = 'on' + capitalize(toIdent(event.name));

    yield `${onName} :: forall m s. (DomEvent -> Mation m s) -> Prop m s`;
    yield `${onName} = mkListener "${event.name}"`;
    yield '';
  }

  function capitalize(str) {
    return str.slice(0, 1).toUpperCase() + str.slice(1);
  }
}

function * styles() {
  yield 'module Mation.Gen.Styles where';
  yield '';
  yield 'import Prelude';
  yield 'import Mation.Core.Style (Style, mkStyle)';
  yield '';
  yield '';

  const { styles } = require('./data/styles.js');
  for (const style of styles) {
    const ident = toIdent(style.name);

    yield `${ident} :: String -> Style`;
    yield `${ident} = mkStyle "${style.name}"`
    yield '';
  }
}


main();
