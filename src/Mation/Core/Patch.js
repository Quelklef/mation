
function iife(f) {
  return f();
}

function replaceNode(target, replacement) {
  if (target instanceof Element) {
    target.replaceWith(replacement);
  } else if (target.parentNode) {
    target.parentNode.replaceChild(replacement, target);
  } else {
    throw Error("Your DOM consists of a single node which isn't an Element?");
  }
}

export const patch_f =
({ caseMaybe
 , caseUnsure
 , caseVNode
 , mPruneMap
}) => {

  const pruneMap = caseMaybe(mPruneMap)({})(x => x);

  return ({ mOldVNode, newVNode }) => root => () => {
    const mOldVNode_ = caseMaybe(mOldVNode)(undefined)(x => x);
    patch(root, mOldVNode_, newVNode);
    return pruneMap;
  };

  // Create an empty VTag
  function mkEmptyVTag(tag){
    return { tag, attrs: [], listeners: [], children: [], fixup: () => {} };
  }

  // Mutatively patches the given root node AND returns the new root node
  function patch(root, mOldVNode, newVNode) {

    // Perform patch
    const result = (
      caseVNode(newVNode)
        (domNode => {
          replaceNode(root, domNode);
          return domNode;
        })
        (html => {
          const $div = document.createElement('div');
          $div.innerHTML = html;
          $div.style.display = 'contents';
          replaceNode(root, $div);
          return $div;
        })
        (text => {
          const node = document.createTextNode(text);
          replaceNode(root, node);
          return node;
        })
        (newVTag => {
          return tagCase(root, mOldVNode, newVTag)
        })
        (vPrune => {
          return pruneCase(root, mOldVNode, vPrune)
        })
    );

    // Let node know that we've visited it
    // This is undocumented and only exists for use in one of the test cases
    // This should otherwise never be used
    if (root._touch)
      root._touch();

    return result;

  }

  function pruneCase(root, mOldVNode, vPrune) {

    // Injectively fold the pruning key path into a single string
    const key = vPrune.keyPath.map(k => k.replace(/,/g, ',,')).join(',;');

    const info = lookupPrune(pruneMap, vPrune);
    if (info) {
      root.replaceWith(info.node);
      return info.node;
    } else {
      const newVNode = vPrune.render(vPrune.params);
      const node = patch(root, mOldVNode, newVNode);
      pruneMap[key] = { params: vPrune.params, node, vNode: newVNode };
      return node;
    }

  }

  // Lookup a vPrune node in the prune map
  // Returns either null or the prune map data object
  function lookupPrune(pruneMap, vPrune) {
    // Injectively fold the pruning key path into a single string
    const key = vPrune.keyPath.map(k => k.replace(/,/g, ',,')).join(',;');

    const info = pruneMap[key];
    if (!info) return null;
    const unsureEq = vPrune.unsureEq;
    const paramsEqual = caseUnsure(unsureEq(vPrune.params)(info.params))(x => x)(false);
    return paramsEqual ? info : null;
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
        (vPrune => newVTag.tag !== 'span')
    );
    if (shouldReplace) {
      const newRoot = document.createElement(newVTag.tag);
      root.replaceWith(newRoot)
      root = newRoot;
      mOldVNode = null;  // Need to diff afresh
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
          const info = lookupPrune(pruneMap, vPrune);

          // The prune was rendered last frame, so it must be in the prune map
          console.assert(!!info, `[mation] prune missing from map`);

          // You and I have magic knowledge that all VPrune nodes render to VTag nodes
          const vTag = caseVNode(info.vNode)(_ => null)(_ => null)(_ => null)(x => x)(_ => null);

          return vTag;
        })
    );

    patchAttrs(root, oldVTag.attrs, newVTag.attrs);
    patchListeners(root, oldVTag.listeners, newVTag.listeners);
    patchChildren(root, oldVTag.children, newVTag.children);

    const { restore } = newVTag.fixup(root)();
    root._fixupRestore = restore;

    return root;
  }

  function patchAttrs(root, oldAttrs, newAttrs) {
    // Add new attrs + modify changed attrs
    for (const [name, value] of newAttrs) {
      root.setAttribute(name, value);
    }

    // Remove old attrs
    const newAttrNames = new Set(newAttrs.map(([name, _]) => name));
    for (const [name, _] of oldAttrs) {
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
    for (const [name, _] of oldListeners) {
      const map = root._listeners ?? {};
      root.removeEventListener(name, map[name]);
    }

    // Add new listeners
    root._listeners = {};
    for (const [name, f] of newListeners) {
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
