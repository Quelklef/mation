
export const onBody =
() => {
  return document.body;
};

export const underBody =
() => {
  const root = document.body;
  if (root.childNodes.length < 1)
    root.append('');
  return root.childNodes[0];
};

export const onHtml =
() => {
  return document.html;
};
