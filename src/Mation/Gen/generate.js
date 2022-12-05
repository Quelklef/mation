const fs = require('fs');
const plib = require('path');

function main() {
  fs.writeFileSync(plib.resolve(__dirname, 'Tags.purs'), [...tags()].join('\n'));
  fs.writeFileSync(plib.resolve(__dirname, 'Attributes.purs'), [...attributes()].join('\n'));
  fs.writeFileSync(plib.resolve(__dirname, 'Events.purs'), [...events()].join('\n'));
}


// Convert a string to a similar purescript identifier
function toIdent(str) {
  const psKeywords = new Set(['class', 'data', 'type']);

  let ident = str;
  ident = ident.replace(/-/g, '');
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
  yield 'import Mation.Core.Html (Prop (..))';
  yield '';
  yield '';

  const { attributes } = require('./data/attributes.js');
  for (const attr of attributes) {
    const ident = toIdent(attr.name);

    if (attr.isBoolean) {
      yield `${ident} :: forall m s. Boolean -> Prop m s`;
      yield `${ident} bool = if bool then PPair "${attr.name}" "${attr.name}" else PNoop`;
    } else {
      yield `${ident} :: forall m s. String -> Prop m s`;
      yield `${ident} val = PPair "${attr.name}" val`;
    }
    yield '';
  }
}

function * events() {
  yield 'module Mation.Gen.Events where';
  yield '';
  yield 'import Mation.Core.Mation (Mation)'
  yield 'import Mation.Core.Html (DOMEvent, Prop (..))';
  yield '';
  yield '';

  const { events } = require('./data/events.js');
  for (const event of events) {
    const onName = 'on' + capitalize(toIdent(event.name));

    yield `${onName} :: forall m s. (DOMEvent -> Mation m s) -> Prop m s`;
    yield `${onName} = PListener "${event.name}"`;
    yield '';
  }

  function capitalize(str) {
    return str.slice(0, 1).toUpperCase() + str.slice(1);
  }
}


main();
