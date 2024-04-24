
export const primEq =
a => b => {
  return a === b;
};

export const unsureEqArrayImpl_f =
({ unsureEq, and, isSurelyFalse, surelyTrue }) =>
as => bs => {
  let result = surelyTrue;
  for (let i = 0; i < as.length; i++){
    const eq = unsureEq(as[i])(bs[i]);
    if (isSurelyFalse(eq)) return eq;
    result = and(result)(eq);
  }
  return result;
};
