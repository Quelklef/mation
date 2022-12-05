
export const useBody =
() => {
  const body = document.body;
  if (body.childNodes.length < 1)
    body.append('');
  return body.childNodes[0];
};
