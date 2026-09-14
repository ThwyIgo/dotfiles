<#import "template.ftl" as layout>
<@layout.registrationLayout displayInfo=false displayMessage=!messagesPerField.existsError('username'); section>
    <#if section = "breadcrumb">
        <nav class="portal-breadcrumb">
            <a href="${url.loginUrl}">${msg("home")}</a>
            <i class="separator">&gt;</i>
            <span class="current">${msg("emailForgotTitle")}</span>
        </nav>
    <#elseif section = "owl">
        <div class="portal-owl">
            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 124 95" width="130" height="95" fill="#FBFEF9">
                <path d="M-9.59635e-06 0.401295C2.14692 4.23767 5.75456 10.0364 11.2403 16.2083C18.4716 24.3425 23.2349 26.666 45.0293 44.6962C50.2862 49.0462 58.1997 55.7759 59.6845 66.0571C59.8129 66.956 59.8691 67.6984 59.8932 68.1759C59.1829 69.2514 58.284 70.9649 57.987 73.1961C57.2286 78.9427 61.1292 83.1924 61.7753 83.8746C62.3291 83.2927 66.8797 78.3568 65.5394 72.3695C65.1301 70.5396 64.2834 69.123 63.5209 68.1318C63.5169 67.6783 63.5209 67.0042 63.5811 66.1975C64.3355 55.9164 72.0243 48.5526 78.6537 42.8382C95.9094 27.9662 101.804 27.8619 111.74 17.3199C117.904 10.7748 121.745 4.32997 124 0C120.148 2.00647 116.367 4.11327 112.35 5.80673C81.6353 18.7645 39.3871 18.6161 8.9609 4.88777C5.91508 3.51534 3.03779 1.80984 0.00400727 0.401295H-9.59635e-06Z" fill="#FBFEF9"/>
                <path d="M5.22075 21.6699C3.84431 27.2158 2.58425 33.4118 1.6372 40.1977C0.690141 46.9836 0.200563 53.2879 0.00794161 58.9983C0.750336 56.3297 2.82904 50.2501 8.51137 44.8768C13.9288 39.7522 19.7757 37.9665 22.4724 37.3164C19.6995 35.3019 16.7379 32.9423 13.6961 30.2014C10.4416 27.268 7.62852 24.3786 5.22477 21.6699H5.22075Z" fill="#FBFEF9"/>
                <path d="M118.731 21.6699C120.107 27.2158 121.367 33.4118 122.314 40.1977C123.261 46.9836 123.751 53.2879 123.944 58.9983C123.201 56.3297 121.123 50.2501 115.44 44.8768C110.023 39.7522 104.176 37.9665 101.479 37.3164C104.252 35.3019 107.214 32.9423 110.255 30.2014C113.51 27.268 116.323 24.3786 118.727 21.6699H118.731Z" fill="#FBFEF9"/>
                <circle id="eye-left" cx="93.79" cy="67" r="13" fill="#FBFEF9"/>
                <path d="M93.7223 48.4523C83.3689 48.4523 74.9498 56.8755 74.9498 67.2248C74.9498 77.5742 83.3729 85.9974 93.7223 85.9974C104.072 85.9974 112.495 77.5742 112.495 67.2248C112.495 56.8755 104.072 48.4523 93.7223 48.4523ZM93.7223 43.6367C106.752 43.6367 117.31 54.1988 117.31 67.2248C117.31 80.2509 106.748 90.8129 93.7223 90.8129C80.6963 90.8129 70.1342 80.2509 70.1342 67.2248C70.1342 54.1988 80.6963 43.6367 93.7223 43.6367Z" fill="#FBFEF9"/>
                <circle id="eye-right" cx="28.13" cy="67" r="13" fill="#FBFEF9"/>
                <path d="M28.0707 48.4523C17.7173 48.4523 9.29811 56.8755 9.29811 67.2248C9.29811 77.5742 17.7213 85.9974 28.0707 85.9974C38.42 85.9974 46.8432 77.5742 46.8432 67.2248C46.8432 56.8755 38.42 48.4523 28.0707 48.4523ZM28.0707 43.6367C41.1007 43.6367 51.6588 54.1988 51.6588 67.2248C51.6588 80.2509 41.0967 90.8129 28.0707 90.8129C15.0446 90.8129 4.48257 80.2509 4.48257 67.2248C4.48257 54.1988 15.0446 43.6367 28.0707 43.6367Z" fill="#FBFEF9"/>
            </svg>
        </div>
    <#elseif section = "header">
        <h2 class="portal-title">${msg("emailForgotTitle")}</h2>
        <p class="portal-subtitle">${msg("emailInstruction")}</p>
    <#elseif section = "form">
        <form id="kc-reset-password-form" class="portal-form" action="${url.loginAction}" method="post">
            <input type="hidden" id="username" name="username" value="${(auth.attemptedUsername!'')}" />

            <div class="portal-form-fields">
                <div class="portal-form-group">
                    <div class="portal-input-wrapper <#if messagesPerField.existsError('username')>has-error</#if>">
                        <input type="text" id="recovery-email" autofocus placeholder=" " value="${(auth.attemptedUsername!'')}" />
                        <label for="recovery-email" class="portal-input-label">${msg("email")}</label>
                        <span class="material-icons portal-input-icon">mail_outline</span>
                    </div>
                </div>

                <div class="portal-or-divider">${msg("orDivider")}</div>

                <div class="portal-form-group">
                    <div class="portal-input-wrapper">
                        <input type="text" id="recovery-phone" placeholder=" " />
                        <label for="recovery-phone" class="portal-input-label">${msg("phoneOrWhatsapp")}</label>
                        <span class="material-icons portal-input-icon">phone</span>
                    </div>
                </div>

                <#if messagesPerField.existsError('username')>
                    <span class="portal-error-msg">${kcSanitize(messagesPerField.get('username'))?no_esc}</span>
                </#if>
            </div>

            <button class="portal-btn" type="submit">${msg("sendPassword")}</button>
        </form>
    </#if>
</@layout.registrationLayout>
