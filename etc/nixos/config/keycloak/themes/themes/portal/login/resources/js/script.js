document.addEventListener('DOMContentLoaded', () => {
    // Password visibility toggle
    document.querySelectorAll('.password-toggle').forEach(toggleBtn => {
        toggleBtn.addEventListener('click', () => {
            const targetId = toggleBtn.getAttribute('data-target');
            const input = document.getElementById(targetId);
            if (!input) return;

            if (input.type === 'password') {
                input.type = 'text';
                toggleBtn.textContent = 'visibility_off';
            } else {
                input.type = 'password';
                toggleBtn.textContent = 'visibility';
            }
        });
    });

    // Admin contact view toggle on registration screen
    const adminLink = document.getElementById('link-admin-contact');
    const adminBox = document.getElementById('portal-admin-box');
    const registerForm = document.getElementById('kc-register-form');
    const backToFormBtn = document.getElementById('btn-back-to-form');
    const breadcrumbCurrent = document.getElementById('breadcrumb-current');

    if (adminLink && adminBox && registerForm) {
        adminLink.addEventListener('click', (e) => {
            e.preventDefault();
            registerForm.style.display = 'none';
            adminBox.classList.add('active');
            if (breadcrumbCurrent) breadcrumbCurrent.textContent = 'Administradores';
        });

        if (backToFormBtn) {
            backToFormBtn.addEventListener('click', () => {
                adminBox.classList.remove('active');
                registerForm.style.display = 'flex';
                if (breadcrumbCurrent) breadcrumbCurrent.textContent = 'Solicitar acesso';
            });
        }
    }

    // Reset password form handler
    const resetForm = document.getElementById('kc-reset-password-form');
    const emailInput = document.getElementById('recovery-email');
    const phoneInput = document.getElementById('recovery-phone');
    const hiddenUsername = document.getElementById('username');

    if (resetForm && emailInput && phoneInput && hiddenUsername) {
        resetForm.addEventListener('submit', (e) => {
            const emailVal = emailInput.value.trim();
            const phoneVal = phoneInput.value.trim();

            if (emailVal) {
                hiddenUsername.value = emailVal;
            } else if (phoneVal) {
                hiddenUsername.value = phoneVal;
            } else {
                e.preventDefault();
                alert('É necessário preencher um dos campos.');
            }
        });
    }

    // Registration form password match validation
    const regForm = document.getElementById('kc-register-form');
    const password = document.getElementById('password');
    const passwordConfirm = document.getElementById('password-confirm');

    if (regForm && password && passwordConfirm) {
        regForm.addEventListener('submit', (e) => {
            if (password.value !== passwordConfirm.value) {
                e.preventDefault();
                let err = document.getElementById('password-mismatch-error');
                if (!err) {
                    err = document.createElement('span');
                    err.id = 'password-mismatch-error';
                    err.className = 'portal-error-msg';
                    err.textContent = 'As senhas não coincidem.';
                    passwordConfirm.closest('.portal-form-group').appendChild(err);
                }
            }
        });
    }

    // Owl eye animation (shrinking circle radius centered at cx/cy)
    document.querySelectorAll('.portal-owl').forEach(container => {
        const leftEye = container.querySelector('#eye-left');
        const rightEye = container.querySelector('#eye-right');
        if (!leftEye || !rightEye) return;

        let animFrame = null;
        function animateRadius(targetR, duration = 200) {
            if (animFrame) cancelAnimationFrame(animFrame);
            const startLeft = parseFloat(leftEye.getAttribute('r')) || 13;
            const startRight = parseFloat(rightEye.getAttribute('r')) || 13;
            const startTime = performance.now();

            function step(now) {
                const elapsed = now - startTime;
                const progress = Math.min(elapsed / duration, 1);
                const ease = progress < 0.5 ? 2 * progress * progress : 1 - Math.pow(-2 * progress + 2, 2) / 2;
                const currentLeft = startLeft + (targetR - startLeft) * ease;
                const currentRight = startRight + (targetR - startRight) * ease;

                leftEye.setAttribute('r', currentLeft.toFixed(2));
                rightEye.setAttribute('r', currentRight.toFixed(2));

                if (progress < 1) {
                    animFrame = requestAnimationFrame(step);
                }
            }
            animFrame = requestAnimationFrame(step);
        }

        container.addEventListener('mouseenter', () => animateRadius(6.5));
        container.addEventListener('mouseleave', () => animateRadius(13));
    });
});
