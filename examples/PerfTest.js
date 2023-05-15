
export const pow =
n => k => {
  return Math.pow(n, k);
};

export const toFixed =
k => n => {
  if (k === 0)
    return '' + Math.round(n);
  return n.toFixed(k);
};

export const timeMe =
block => () => {
  const t0 = Date.now();
  block();
  const dt = Date.now() - t0;
  return dt;
};
