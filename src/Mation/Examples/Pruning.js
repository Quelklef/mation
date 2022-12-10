
export const prepare =
node => () => {
  // _touch is an undocumented Mation hook
  node._touch = () => {
    node.style.background = 'rgba(255, 100, 100, 0.2)';
    setTimeout(() => {
      node.style.background = ''
    }, 200);
  }
  return { restore: () => {} };
};
