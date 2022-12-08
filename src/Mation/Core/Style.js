
export const putClass =
node => className => () => {
  node.classList.add(className);

  const restore = () => node.classList.remove(className);
  return restore;
};

export const putCss =
({ css, forClass }) => () => {
  const $sheet = document.createElement('style');
  $sheet.innerText = css;

  const hangoutId = '_mation-stylesheet-hangout';
  let $hangout = document.getElementById(hangoutId);
  if (!$hangout) {
    $hangout = document.createElement('div');
    $hangout.id = hangoutId;
    document.head.append($hangout);
  }

  $hangout.append($sheet);

  const restore = () => $sheet.remove();
  return restore;
};
