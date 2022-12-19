
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
 , casePair
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
          $div.style.display = 'contents';
          replaceNode(root, $div);
          $div.innerHtml = html;
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

    // Compute an existing DOM node to re-use for this node, as per pruning rules
    // Returns `null` if the prune node needs be recomputed
    const reuse = iife(() => {
      const info = pruneMap[key];
      if (!info) return null;
      const unsureEq = vPrune.unsureEq;
      const paramsEqual = caseUnsure(unsureEq(vPrune.params)(info.params))(x => x)(false);
      return paramsEqual ? info.node : null;
    });

    if (reuse) {
      root.replaceWith(reuse);
      return reuse;
    } else {
      const newVNode = vPrune.render(vPrune.params);
      const node = patch(root, mOldVNode, newVNode);
      pruneMap[key] = { params: vPrune.params, node };
      return node;
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

    return root;
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
