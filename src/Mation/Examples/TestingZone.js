
export const repeatedly =
eff => () => {
  eff();
  const id = setInterval(eff, 20);
  const cancel = () => clearInterval(id);
  return { cancel };
};


export const parseInt =
s => {
  let n = +s;
  if (!Number.isFinite(n) && (n | 0) === n)
    n = 0;
  return n;
};

export const mkTextRawNode =
() => {

  const $node = document.createElement('div');
  $node.style.margin = '1em 0';
  $node.style.padding = '1em';
  $node.style.border = '1px dashed green';

  $node.append('upper(')

  const $input = document.createElement('input');
  $node.append($input);

  $node.append(') = ');

  const $output = document.createElement('span');
  $node.append($output);

  $input.addEventListener('input', () => {
    $output.innerText = $input.value.toUpperCase();
  });

  return $node;

};

export const mkIframeRawNode =
() => {

  const $node = document.createElement('div');
  $node.style.margin = '1em 0';
  $node.style.padding = '1em';
  $node.style.border = '1px dashed green';

  $node.append('iframe(')

  const $input = document.createElement('input');
  $node.append($input);

  $node.append(') = ');

  const $output = document.createElement('iframe');
  $output.style.verticalAlign = 'middle';
  $output.style.border = '1px solid grey';
  $output.style.width = '400px';
  $output.style.height = '200px';
  $node.append($output);

  $input.addEventListener('input', () => {
    $output.src = $input.value.toUpperCase();
  });

  return $node;

};


