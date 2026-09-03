export function splitLeadingSpaces(source) {
  let index = 0;
  while (index < source.length && source.charCodeAt(index) === 32) {
    index += 1;
  }
  return [index, source.slice(index)];
}
