
export const repeat =
eff => () => {
  eff();
  const id = setInterval(eff, 200);
  return () => clearInterval(id);
};
