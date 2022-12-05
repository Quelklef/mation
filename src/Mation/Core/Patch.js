
export const patch_f =
caseMaybe => ({ mOldHtml, newHtml }) => root => {
  return (
    caseMaybe
      (mOldHtml)
      (() => root.replaceWith(makeAnew(newHtml)))
      (oldHtml => () => patch(root, oldHtml, newHtml))
  );
};

function makeAnew(html) {
  return (
    html
      (node => node)
      (html => {
        const root = document.createElement('div');
        root.innerHTML = html;
        root.style.display = 'contents';
        return root;
      })
      (text => document.createTextNode(text))
      (info => {
        const { tag, attrs, listeners, fixup, children } = info;

        const node = document.createElement(tag);

        for (const { name, value } of attrs) {
          node.setAttribute(name, value);
        }

        node._listeners = {};
        for (const listener of listeners) {
          const handler = ev => listener.handler(ev)();
          node._listeners[listener.name] = handler;
          node.addEventListener(listener.name, handler);
        }

        for (const child of children) {
          node.append(makeAnew(child));
        }

        fixup(node)();

        return node;
      })
  );
}


function patch(root, oldHtml, newHtml) {

  (
    newHtml
      (node => root.replaceWith(node))
      (html => root.innerHTML = html)
      (text => root.replaceWith(text))
      (tagCase)
  );

  function tagCase(newInfo) {

    // If tag type has changed, replace root
    const shouldReplace = (
      oldHtml
        (node => true)
        (html => true)
        (text => true)
        (oldInfo => oldInfo.tag !== newInfo.tag)
    );
    if (shouldReplace) {
      root.replaceWith(document.createElement(tag))
    }

    const emptyInfo = { tag: newInfo.tag, attrs: [], listeners: [], children: [], fixup: () => {} };
    const oldInfo = (
      oldHtml
        (node => emptyInfo)
        (html => emptyInfo)
        (text => emptyInfo)
        (oldInfo => oldInfo)
    );

    patchAttrs(root, oldInfo.attrs, newInfo.attrs);
    patchListeners(root, oldInfo.listeners, newInfo.listeners);
    patchChildren(root, oldInfo.children, newInfo.children);

    newInfo.fixup(root)();

  }

}

function patchAttrs(root, oldAttrs, newAttrs) {
  // Add new attrs + modify changed attrs
  for (const attr of newAttrs) {
    root.setAttribute(attr.name, attr.value);
  }

  // Remove old attrs
  const newAttrNames = new Set(newAttrs.map(({ name }) => name));
  for (const attr of oldAttrs) {
    if (!newAttrNames.has(attr.name)) {
      root.removeAttribute(attr.name);
    }
  }
}

function patchListeners(root, oldListeners, newListeners) {
  // Can't diff functions, so just remove all old listeners then add the new

  // Remove old listeners
  for (const listener of oldListeners) {
    const map = root._listeners ?? {};
    root.removeEventListener(listener.name, map[listener.name]);
  }

  // Add new listeners
  root._listeners = {};
  for (const listener of newListeners) {
    const handler = ev => listener.handler(ev)();
    root.addEventListener(listener.name, handler);
    root._listeners[listener.name] = handler;
  }
}

function patchChildren(root, oldChildren, newChildren) {
  // nb. No keyed elements at the moment

  // Add new children + patch existing ones
  for (let i = 0; i < newChildren.length; i++) {
    // Ensure that a child exists
    if (root.childNodes.length < i) root.append('');
    // Patch the child
    const child = root.childNodes[i];
    patch(child, oldChildren[i], newChildren[i]);
  }

  // Remove excess children
  for (let i = newChildren.length; i < oldChildren.length; i++) {
    root.childNodes[i].remove();
  }
}

