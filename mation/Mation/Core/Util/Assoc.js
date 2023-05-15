
export const mkPair =
a => b => {
  return [a, b];
};

export const usePair =
f => ([a, b]) => {
  return f(a)(b);
};
