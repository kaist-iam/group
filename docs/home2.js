document.addEventListener("DOMContentLoaded", () => {
  // ===== HERO SLIDER =====
  const slider = document.querySelector(".hero-slider");
  if (slider) {
    const slides = Array.from(slider.querySelectorAll(".slide"));
    const dotsWrap = slider.querySelector(".slide-dots");
    const prevBtn = slider.querySelector(".prev");
    const nextBtn = slider.querySelector(".next");

    let index = 0;

    slides.forEach((_, i) => {
      const dot = document.createElement("button");
      dot.className = "slide-dot" + (i === 0 ? " is-active" : "");
      dot.type = "button";
      dot.addEventListener("click", () => goTo(i));
      dotsWrap.appendChild(dot);
    });

    const dots = Array.from(dotsWrap.children);

    function goTo(i) {
      slides[index].classList.remove("is-active");
      dots[index].classList.remove("is-active");
      index = i;
      slides[index].classList.add("is-active");
      dots[index].classList.add("is-active");
    }

    function next() { goTo((index + 1) % slides.length); }
    function prev() { goTo((index - 1 + slides.length) % slides.length); }

    nextBtn?.addEventListener("click", next);
    prevBtn?.addEventListener("click", prev);

    let timer = setInterval(next, 6500);
    slider.addEventListener("mouseenter", () => clearInterval(timer));
    slider.addEventListener("mouseleave", () => (timer = setInterval(next, 6500)));
  }

  // ===== MEMBERS AUTO-SCROLL =====
  const scrollEl = document.getElementById("membersScroll");
  if (scrollEl) {
    let paused = false;
    const speed = 0.35;

    function autoScroll() {
      if (!paused) {
        scrollEl.scrollLeft += speed;
        if (scrollEl.scrollLeft + scrollEl.clientWidth >= scrollEl.scrollWidth - 2) {
          scrollEl.scrollLeft = 0;
        }
      }
      requestAnimationFrame(autoScroll);
    }

    scrollEl.addEventListener("mouseenter", () => (paused = true));
    scrollEl.addEventListener("mouseleave", () => (paused = false));
    autoScroll();
  }
});