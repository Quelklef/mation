
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
    if (!(hash in refs)) {
      console.warn('[mation] unexpected absence from css refmap');
      return;
    }
    refs[hash].count--;
    if (refs[hash].count === 0) {
      refs[hash].elem.remove();
      delete refs[hash];
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
    $hangout.dataset.mationNoPatch = true;
    document.head.append($hangout);
  }
  return $hangout;
}
