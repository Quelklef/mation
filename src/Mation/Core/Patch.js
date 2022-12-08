
export const patch_f =
caseMaybe => casePair => caseVNode => {

  return ({ mOldVNode, newVNode }) => root => () => {
    const nOldVNode = caseMaybe(mOldVNode)(undefined)(x => x);
    patch(root, nOldVNode, newVNode);
  };

  // Create an empty VTag
  function mkEmptyVTag(tag){
    return { tag, attrs: [], listeners: [], children: [], fixup: () => {} };
  }

  function patch(root, mOldVNode, newVNode) {
    caseVNode(newVNode)
      (domNode => root.replaceWith(domNode))
      (html => root.innerHTML = html)
      (text => root.replaceWith(text))
      (newVTag => tagCase(root, mOldVNode, newVTag))
  }

  function tagCase(root, mOldVNode, newVTag) {
    // mOldVNode may be nully

    // Perform fixup-restore from last frame
    if (root._fixupRestore)
      root._fixupRestore();

    // If root is not a tag of correct type, replace it
    const shouldReplace = (
      !mOldVNode ||
      caseVNode(mOldVNode)
        (domNode => true)
        (html => true)
        (text => true)
        (oldVTag => oldVTag.tag !== newVTag.tag)
    );
    if (shouldReplace) {
      const newRoot = document.createElement(newVTag.tag);
      root.replaceWith(newRoot)
      root = newRoot;
    }

    const empty = mkEmptyVTag(newVTag.tag);
    const oldVTag = (
      !mOldVNode ? empty :
      caseVNode(mOldVNode)
        (domNode => empty)
        (html => empty)
        (text => empty)
        (oldVTag => oldVTag)
    );

    patchAttrs(root, oldVTag.attrs, newVTag.attrs);
    patchListeners(root, oldVTag.listeners, newVTag.listeners);
    patchChildren(root, oldVTag.children, newVTag.children);

    const { restore } = newVTag.fixup(root)();
    root._fixupRestore = restore;
  }

  function patchAttrs(root, oldAttrs, newAttrs) {
    // Add new attrs + modify changed attrs
    for (const attr of newAttrs) {
      const [name, value] = casePair(attr)(a => b => [a, b]);
      root.setAttribute(name, value);
    }

    // Remove old attrs
    const newAttrNames = new Set(newAttrs.map(attr => casePair(attr)(name => _ => name)));
    for (const attr of oldAttrs) {
      const [name, _] = casePair(attr)(a => b => [a, b]);
      if (!newAttrNames.has(name)) {
        root.removeAttribute(name);
      }
    }
  }

  function patchListeners(root, oldListeners, newListeners) {
    // Can't diff functions, so just remove all old listeners then add the new

    // Remove old listeners
    // Read listeners from oldListeners instead of from root._listeners so that
    // we don't remove listeners added by other code
    for (const listener of oldListeners) {
      const [name, _] = casePair(listener)(a => b => [a, b]);
      const map = root._listeners ?? {};
      root.removeEventListener(name, map[name]);
    }

    // Add new listeners
    root._listeners = {};
    for (const listener of newListeners) {
      const [name, handler] = casePair(listener)(a => b => [a, b]);
      const domHandler = ev => handler(ev)();
      root.addEventListener(name, domHandler);
      root._listeners[name] = domHandler;
    }
  }

  function patchChildren(root, oldChildren, newChildren) {
    // Add new children + patch existing ones
    for (let i = 0; i < newChildren.length; i++) {
      // Ensure that a child exists
      if (root.childNodes.length <= i) root.append('');
      // Patch the child
      const child = root.childNodes[i];
      patch(child, oldChildren[i], newChildren[i]);
    }

    // Remove excess children
    while (root.childNodes.length > newChildren.length) {
      root.lastChild.remove();
    }
  }

};
