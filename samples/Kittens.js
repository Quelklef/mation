
export const generateCat_foreign =
({ width, height }) => async () => {

  // Spoof a longer delay
  await new Promise(resolve => setTimeout(resolve, 3500));

  // Generate the kitten
  const resp = await fetch(`https://placekitten.com/${width}/${height}`);
  const blob = await resp.blob();

  // Transform to data uri
  const dataUri = await new Promise(resolve => {
    const reader = new FileReader();
    reader.onload = () => resolve(reader.result);
    reader.readAsDataURL(blob);
  });

  return { html: `<img src="${dataUri}" />` };

};
