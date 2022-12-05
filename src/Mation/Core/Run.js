
export const useBody =
() => {
  const root = document.body;
  if (root.childNodes.length < 1)
    root.append('');
  return root.childNodes[0];
};

export const useHtml =
() => {
  return document.html;
};
