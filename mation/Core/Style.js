
export const putClass =
node => className => () => {
  node.classList.add(className);

  const restore = () => node.classList.remove(className);
  return { restore };
};

export const putCss =
({ getCss, hash }) => () => {
  const $hangout = getHangout();

  // refcounting :)
  const refs = ($hangout._refs ??= {});
  refs[hash] ??= { elem: null, count: 0 };

  if (refs[hash].count === 0) {
    const $sheet = document.createElement('style');
    $sheet.innerText = getCss();
    $hangout.append($sheet);
    refs[hash].elem = $sheet;
  }
  refs[hash].count++;

  const restore = () => {
    refs[hash].count--;
    if (refs[hash].count === 0) {
      refs[hash].elem.remove();
      refs[hash].elem = null;
    }
  }

  return { restore };
};

function getHangout() {
  const hangoutId = 'mation-stylesheet-hangout';
  let $hangout = document.getElementById(hangoutId);
  if (!$hangout) {
    $hangout = document.createElement('div');
    $hangout.id = hangoutId;
    document.head.append($hangout);
  }
  return $hangout;
}
