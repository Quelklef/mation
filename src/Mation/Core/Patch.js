
export const patch_f =
({ caseMaybe
 , casePair
 , caseUnsure
 , caseVNode
}) => {

  return ({ mOldVNode, newVNode }) => root => () => {
    const mOldVNode_ = caseMaybe(mOldVNode)(undefined)(x => x);
    patch(root, mOldVNode_, newVNode);
  };

  // Create an empty VTag
  function mkEmptyVTag(tag){
    return { tag, attrs: [], listeners: [], children: [], fixup: () => {} };
  }

  function patch(root, mOldVNode, newVNode) {

    // Perform patch
    caseVNode(newVNode)
      (domNode => root.replaceWith(domNode))
      (html => root.innerHTML = html)
      (text => root.replaceWith(text))
      (newVTag => tagCase(root, mOldVNode, newVTag))
      (vPrune => pruneCase(root, mOldVNode, vPrune))

    // Let node know that we've visited it
    // This is undocumented and only exists for use in one of the test cases
    // This should otherwise never be used
    if (root._touch)
      root._touch();

  }

  function pruneCase(root, mOldVNode, vPrune) {
    if (!mOldVNode) {
      patch(root, mOldVNode, vPrune.render(vPrune.params));
    } else {
      const oldVNode = mOldVNode;
      caseVNode(oldVNode)
        (domNode => patch(root, oldVNode, vPrune.render(vPrune.params)))
        (html =>    patch(root, oldVNode, vPrune.render(vPrune.params)))
        (text =>    patch(root, oldVNode, vPrune.render(vPrune.params)))
        (vTag =>    patch(root, oldVNode, vPrune.render(vPrune.params)))
        (vPrune2 => {
          // Both nodes are VPrune

          const oldVPrune = vPrune2;
          const newVPrune = vPrune;

          const oldKey = caseMaybe(oldVPrune.mKey)(null)(x => x);
          const newKey = caseMaybe(newVPrune.mKey)(null)(x => x);
          const keysWantPatch = (
            (oldKey === null) !== (newKey === null)
            || (oldKey !== null && oldKey !== newKey)
          );

          const unsureEq = newVPrune.unsureEq;  // should be same as oldVPrune.unsureEq
          const paramsMaybeDiffer = caseUnsure(unsureEq(newVPrune.params)(oldVPrune.params))(x => !x)(true);

          const shouldPatch = keysWantPatch || paramsMaybeDiffer;

          if (shouldPatch)
            patch(root, oldVPrune.render(oldVPrune.params), newVPrune.render(newVPrune.params));
        });
    }
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
        (vPrune => {
          const forced = vPrune.render(vPrune.params);  // FIXME: does this mean VPrune might be computed more than once?
          const oldVTag = caseVNode(forced)(_ => null)(_ => null)(_ => null)(x => x)(_ => null);  // FIXME
          return oldVTag.tag !== newVTag.tag;
        })
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
        (vPrune => {
          const forced = vPrune.render(vPrune.params);  // FIXME: does this mean VPrune might be computed more than once?;
          const oldVTag = caseVNode(forced)(_ => null)(_ => null)(_ => null)(x => x)(_ => null);  // FIXME
          return oldVTag;
        })
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
      const [name, f] = casePair(listener)(a => b => [a, b]);
      const domHandler = ev => f(ev)();
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
