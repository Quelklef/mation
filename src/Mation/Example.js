
export const repeatedly =
eff => () => {
  eff();
  const id = setInterval(eff, 200);
  const cancel = () => clearInterval(id);
  return { cancel };
};
