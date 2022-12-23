
function iife(f) {
  return f();
}

function setNode(target, replacement) {
  if (replacement === target)
    return;  // Seems replacement causes a reflow, which is slow; skip if possible

  if (target instanceof Element) {
    target.replaceWith(replacement);
  } else if (target.parentNode) {
    target.parentNode.replaceChild(replacement, target);
  } else {
    throw Error("Your DOM consists of a single node which isn't an Element?");
  }
}

// An SMap is a mapping whose keys are string arrays
// Values in an SMap must never be nully
class SMap {
  constructor(trie) {
    this.trie = trie || {};
  }

  static rose = Symbol();

  get(keys) {
    let root = this.trie;
    for (const key of keys)
      root = root?.[key];
    return root?.[SMap.rose];
  }

  set(keys, val) {
    let root = this.trie;
    for (const key of keys) {
      if (!(key in root)) root[key] = {};
      root = root[key];
    }
    root[SMap.rose] = val;
  }

  *entries() {
    yield* go([], this.trie);
    function* go(ks, root) {
      if (SMap.rose in root)
        yield [ks, root[SMap.rose]];
      for (const [k, v] of Object.entries(root))
        if (k !== SMap.rose)
          yield* go([...ks, k], v);
    }
  }

  *keys() {
    for (const [ks, v] of this.entries())
      yield ks;
  }

  // Merge another SMap into this one
  merge(other) {
    // Could be more efficient
    for (const [ks, v] of other.entries()) {
      this.set(ks, v);
    }
  }

  // Return an SMap consisting of all (keys, value) pairs of
  // this SMap whose key sequence has the given key sequence
  // as a prefix
  filterPrefixedBy(keys) {
    let root = this.trie;
    for (const key of keys)
      root = root?.[key];
    for (const key of Array.from(keys).reverse())
      root = { [key]: root };
    return new SMap(root);
  }
}


export const patch_f =
({ caseMaybe
 , caseUnsure
 , caseVNode
 , mPruneMap
}) => {

  const oldPruneMap = caseMaybe(mPruneMap)(new SMap())(x => x);
  const newPruneMap = new SMap();

  return ({ mOldVNode, newVNode }) => root => () => {
    const mOldVNode_ = caseMaybe(mOldVNode)(undefined)(x => x);
    patch(root, mOldVNode_, newVNode);
    return newPruneMap;
  };

  // Create an empty VTag
  function mkEmptyVTag(tag){
    return { tag, attrs: [], listeners: [], children: [], fixup: () => {} };
  }

  // Mutatively patches the given root node (ie, target node to mount on)
  // Returns the node that was actually mounted on, which may be different
  //
  // Remark: the diffing algorithm tries its best to perform no unnecessary
  // mutations, such as setting a DOM node attribute to the value it already
  // has. The primary reason for this is that such mutations
  // can interact poorly with parts of the DOM state. For instance, if
  // the user is selecting part of a block of text and the text node gets
  // replaced, the user selection will be disrupted, even if the new text
  // node is equivalent with the previous one.
  // This behaviour also improves performance! (in most cases)
  function patch(root, mOldVNode, newVNode) {

    // Perform patch
    const result = (
      caseVNode(newVNode)
        (domNode => {
          setNode(root, domNode);
          return domNode;
        })
        (html => {
          const alreadyGood = (
            root.tagName === 'DIV'
            && root.style.cssText === 'display: contents;'
            && root.innerHTML === html
          );
          if (alreadyGood) {
            return root;
          } else {
            const $div = document.createElement('div');
            $div.innerHTML = html;
            $div.style.display = 'contents';
            setNode(root, $div);
            return $div;
          }
        })
        (text => {
          const alreadyGood = (
            root.nodeName === '#text'
            && root.textContent === text
          );
          if (alreadyGood) {
            return root;
          } else {
            const node = document.createTextNode(text);
            setNode(root, node);
            return node;
          }
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
    root._touch?.();

    return result;

  }


  function pruneCase(root, mOldVNode, vPrune) {
    const info = lookupPrune(oldPruneMap, vPrune);
    if (info) {
      setNode(root, info.node);
      newPruneMap.merge(oldPruneMap.filterPrefixedBy(vPrune.keyPath));
      return info.node;
    } else {
      const newVNode = vPrune.render(vPrune.params);
      const mountedOn = patch(root, mOldVNode, newVNode);
      newPruneMap.set(vPrune.keyPath, { params: vPrune.params, node: mountedOn, vNode: newVNode });
      return mountedOn;
    }
  }

  // Lookup a vPrune node in the prune map
  // Returns either null or the prune map data object
  function lookupPrune(pruneMap, vPrune) {
    const info = pruneMap.get(vPrune.keyPath);
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
      setNode(root, newRoot);
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
          const info = lookupPrune(oldPruneMap, vPrune);

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
      setAttribute(root, name, value);
    }

    // Remove old attrs
    const newAttrNames = new Set(newAttrs.map(([name, _]) => name));
    for (const [name, _] of oldAttrs) {
      if (!newAttrNames.has(name)) {
        deleteAttribute(root, name);
      }
    }
  }

  function setAttribute(node, name, value) {
    switch (name) {

    // Some HTML attributes don't get auto-sync'd to the corresponding DOM
    // node, so we need to do it ourselves.
    // Also see <https://javascript.info/dom-attributes-and-properties>

    case 'value': {
      const alreadyGood =
        node.getAttribute('value') === value && node.value === value;
      if (!alreadyGood) {
        node.setAttribute('value', value);
        node.value = value;
      }
      return;
    }

    case 'checked': {
      const alreadyGood =
        node.getAttribute('checked') === value && node.checked === !!value;
      if (!alreadyGood) {
        node.setAttribute('checked', value);
        node.checked = !!value;
      }
      return;
    }

    default: {
      const alreadyGood = node.getAttribute(name) === value;
      if (!alreadyGood)
        node.setAttribute(name, value);
      return;
    }

    }
  }

  function deleteAttribute(node, name) {
    switch (name) {

    case 'value': {
      const alreadyGood = node.value === '';
      if (!alreadyGood)
        node.value = '';
    }

    case 'checked': {
      const alreadyGood = !node.checked;
      if (!alreadyGood)
        node.checked = false;
    }

    default: {
      const alreadyGood = !node.hasAttribute(name);
      if (!alreadyGood)
        node.removeAttribute(name);
    }

    }
  }

  function patchListeners(root, oldListeners, newListeners) {
    // Can't diff functions, so just remove and re-add all listeners

    // Remove old listeners
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
