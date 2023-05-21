
export const getTargetValue =
ev => {
  return ev.target.value;
};

export const dataset_f =
kvs => node => () => {

  const before = { ...node.dataset };
  put({});

  put(Object.fromEntries(kvs));

  return { restore: () => put(before) };

  function put(obj) {
    for (const k in node.dataset)
      delete node.dataset[k];
    for (const k in obj)
      node.dataset[k] = obj[k];
  }

};

export const showUpdates_f =
node => () => {
  // _touch is an undocumented Mation hook
  node._touch = () => {
    node.style.outline = '2px solid red';
    setTimeout(() => {
      node.style.outline = ''
    }, 200);
  }
  return { restore: () => {} };
};

export const onClickElsewhere_f =
f => node => () => {
  let lis; document.addEventListener('click', lis = evt => {
    if (!node.contains(evt.target))
      f(evt)()
  });

  const restore = () => document.removeEventListener('click', lis);
  return { restore };
};
