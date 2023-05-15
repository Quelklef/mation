
export const getTargetValue =
ev => {
  return ev.target.value;
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
