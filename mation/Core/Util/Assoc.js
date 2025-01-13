
export const mkPair =
a => b => {
  return [a, b];
};

export const unPair =
f => ([a, b]) => {
  return f(a)(b);
};
