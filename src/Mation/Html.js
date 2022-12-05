
export const renderHtml_f =
html => () => {

  return renderHtml(html);

  function renderHtml(html) {
    return (
      html
        (node => node)
        (text => document.createTextNode(text))
        (fromInfo)
    );
  }

  function fromInfo({ tag, attrs, listeners, fixup, children }) {

    const node = document.createElement(tag);

    for (const { name, value } of attrs)
      node[name] = value;

    for (const { name, handler } of listeners)
      node.addEventListener(name, handler)

    for (const child of children)
      node.append(renderHtml(child));

    fixup(node)();

    return node;

  }

};
