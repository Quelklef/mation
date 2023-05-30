
// FIXME: It would be good to have some real tests for the fixup
//   lifecycle. Getting the fixup-restore timing and logic
//   correct (or as close to correct as it currently is) has proven
//   rather subtle, and I'm not 100% confident in the current
//   implementation.

export const patch_f =
({ caseMaybe
 , caseUnsure
 , caseVNode
 , collapseRevertible
 , emptyRevertible
 , mPruneMap
}) => {

  const oldPruneMap = caseMaybe(mPruneMap)(Trie_new())(x => x);
  const newPruneMap = Trie_new();

  const oldPruneMapNodes = new Set(mapIter(Trie_values(oldPruneMap), info => info.node));

  return ({ mOldVNode, newVNode }) => root => () => {

    const mOldVNode_ = caseMaybe(mOldVNode)(undefined)(x => x);
    patch(root, mOldVNode_, newVNode);

    // Fixup-release all nodes present in old prune map but not new one
    // Such nodes are detached from the DOM and are to be forgotten
    const newPruneMapNodes = new Set(mapIter(Trie_values(newPruneMap), info => info.node));
    for (const node of oldPruneMapNodes)
      if (!newPruneMapNodes.has(node))
        fixupRelease(node);

    return newPruneMap;
  };

  // Create an empty VTag
  function mkEmptyVTag(tag){
    return { tag, attrs: [], listeners: [], children: [], fixup: emptyRevertible };
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
    const mountedOn = (
      caseVNode(newVNode)
        (domNode => {
          setNode(root, domNode);
          fixupConditionalRelease(root);
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
            fixupConditionalRelease(root);
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
            fixupConditionalRelease(root);
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
    mountedOn._touch?.();

    return mountedOn;

  }


  function pruneCase(root, mOldVNode, vPrune) {
    const info = lookupPrune(oldPruneMap, vPrune);
    if (info) {
      setNode(root, info.node);
      fixupConditionalRelease(root);
      Trie_mergeAt(newPruneMap, oldPruneMap, vPrune.keyPath);
        // ^ Add the prune children, which won't otherwise be seen in this diff
        // FIXME: I think this can be replaced with a shallow merge?
      return info.node;
    } else {
      const newVNode = vPrune.render(vPrune.params);
      const mountedOn = patch(root, mOldVNode, newVNode);
      Trie_set(newPruneMap, vPrune.keyPath, { params: vPrune.params, node: mountedOn, vNode: newVNode });
      return mountedOn;
    }
  }

  // Lookup a vPrune node in the prune map
  // Returns null on absence
  function lookupPrune(pruneMap, vPrune) {
    const info = Trie_get(pruneMap, vPrune.keyPath);
    if (!info) return null;
    const unsureEq = vPrune.unsureEq;
    const paramsEqual = caseUnsure(unsureEq(vPrune.params)(info.params))(x => x)(false);
    return paramsEqual ? info : null;
  }

  // Like lookupPrune but skips checking the prune params
  function lookupPruneUnsafe(pruneMap, vPrune) {
    return Trie_get(pruneMap, vPrune.keyPath);
  }


  function tagCase(root, mOldVNode, newVTag) {
    // mOldVNode may be nully

    // Perform fixup-restore from last frame
    fixupRestore(root);

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
      fixupConditionalRelease(root);
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
          // Since the prune was rendered last frame, it must be in the prune map
          // Also, we need not check the prune params
          const info = lookupPruneUnsafe(oldPruneMap, vPrune);
          console.assert(!!info, `[mation] prune missing from map (looking for keypath '${vPrune.keyPath.map(k => '→ ' + k).join(' ')}')`);
          // You and I have magic knowledge that all VPrune nodes render to VTag nodes
          const vTag = caseVNode(info.vNode)(_ => null)(_ => null)(_ => null)(x => x)(_ => null);
          return vTag;
        })
    );

    patchAttrs(root, oldVTag.attrs, newVTag.attrs);
    patchListeners(root, oldVTag.listeners, newVTag.listeners);
    patchChildren(root, oldVTag.children, newVTag.children);

    // Perform fixup & store restoration function
    const restore = collapseRevertible(newVTag.fixup(root))();
    fixupAttachRestore(root, restore);

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

    case 'selected': {
      const alreadyGood =
        node.getAttribute('selected') === value && node.selected === !!value;
      if (!alreadyGood) {
        node.setAttribute('selected', value);
        node.selected = !!value;
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

    case 'selected': {
      const alreadyGood = !node.selected;
      if (!alreadyGood)
        node.selected = false;
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
    // Patch child list
    // Ignores children with data-mation-no-patch=true

    let di = 0;  // DOM index
    let vi = 0;  // VDOM index

    // Add new children + patch existing ones
    while (vi < newChildren.length) {
      // Ensure that a child exists
      if (di >= root.childNodes.length) root.append('');
      // Fetch the child
      const ch = root.childNodes[di];
      if (ch.dataset?.mationNoPatch) { di++; continue; }
      // Patch the child
      patch(ch, oldChildren[vi], newChildren[vi]);
      di++; vi++;
    }

    // Remove excess children
    while (di < root.childNodes.length) {
      const ch = root.childNodes[di];
      if (ch.dataset?.mationNoPatch) { di++; continue; }
      ch.remove();
      fixupConditionalRelease(ch);
    }

  }


  function fixupAttachRestore(node, restore) {
    node._fixupRestore = restore;
  }

  function fixupRestore(node) {
    if (node._fixupRestore) {
      node._fixupRestore();
      delete node._fixupRestore;
        // ^ This ensures we don't ever double-restore
        //   Strictly speaking doing this should not be necessary, as we could instead
        //   just make sure we never call fixupRestore() twice on the same node within
        //   a single patch. But doing it this way is simpler.
    }
  }

  // Fixup-restore a node and all its descendants
  //
  // Generally this needs to be called whenever we remove a DOM node in order to
  // ensure all of its descendants complete their fixup lifecycle.
  //
  // Exception: if the node appears in the oldPruneMap, then it will not
  // be fixup-released. This is because a node which is removed from the DOM but
  // present in the oldPruneMap might still be re-added to the DOM (in the same
  // frame) later on by a VPrune node, so it is not safe to eagerly fixup-release.
  // At the end of the patching algorithm we fixup-restore the appropriate subset
  // of these dubious nodes by comparing oldPruneMap with the final newPruneMap.
  function fixupConditionalRelease(node) {
    if (!oldPruneMapNodes.has(node))
      fixupRelease(node);
  }

  function fixupRelease(node) {
    fixupRestore(node);
    if (node.children)
      for (const ch of node.children)
        fixupConditionalRelease(ch);
  }

};


function iife(f) {
  return f();
}

function* mapIter(it, f) {
  for (const x of it)
    yield f(x);
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


const Trie_rose = Symbol("Trie_rose");

function Trie_new() {
  return {};
}

function Trie_set(trie, ks, v) {
  Trie_zoom(trie, ks)[Trie_rose] = v;
}

function Trie_get(trie, ks) {
  return Trie_peek(trie, ks)?.[Trie_rose];
}

function Trie_merge(trie, other) {
  if (Trie_rose in other)
    trie[Trie_rose] = other[Trie_rose];

  for (const k in other) {
    if (k === Trie_rose) continue;
    Trie_merge(Trie_zoom(trie, [k]), other[k]);
  }
}

function Trie_mergeAt(trie, other, ks) {
  Trie_merge(Trie_zoom(trie, ks), Trie_zoom(other, ks));
}

// Fetch the sub-trie at a given key path. If none
// exists, returns nully.
function Trie_peek(root, ks) {
  for (const key of ks) {
    root = root[key];
    if (!root) return null;
  }
  return root;
}

// Fetch the sub-trie at a given key path. If none
// exists, create one.
function Trie_zoom(root, ks) {
  for (const key of ks) {
    if (!(key in root))
      root[key] = {};
    root = root[key];
  }
  return root;
}

function* Trie_keys(root) {
  for (const k in root) {
    if (k !== Trie_rose) {
      yield k;
      yield* Trie_keys(root[k]);
    }
  }
}

function* Trie_values(root) {
  if (Trie_rose in root)
    yield root[Trie_rose];
  for (const k in root) {
    if (k !== Trie_rose) {
      yield* Trie_values(root[k]);
    }
  }
}

