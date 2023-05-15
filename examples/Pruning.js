
export const parseInt =
s => {
  let n = +s;
  if (!Number.isFinite(n) || (n | 0) !== n)
    n = 0;
  return n;
};
