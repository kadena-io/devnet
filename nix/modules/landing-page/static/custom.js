document.addEventListener('DOMContentLoaded', (event) => {
    openDetailsIfHashMatches();
});

window.addEventListener('hashchange', (event) => {
    openDetailsIfHashMatches();
});

function openDetailsIfHashMatches() {
    const hash = window.location.hash;
    if (!hash) return;
    const targetElement = document.querySelector(hash);
    if (!targetElement) return;
    let parent = targetElement.parentElement;
    while (parent) {
      if (parent.tagName.toLowerCase() === 'details' && !parent.open) {
          parent.open = true;
          break;
      }
      parent = parent.parentElement;
    }
    requestAnimationFrame(() => {
        targetElement.scrollIntoView({ behavior: "smooth", block: "start" });
    });
}
