<#import "template.ftl" as layout>
<@layout.registrationLayout displayMessage=true displayRequiredFields=false cardClass="card-wide"; section>
    <#if section = "breadcrumb">
        <nav class="portal-breadcrumb">
            <a href="${url.loginUrl}">${msg("home")}</a>
            <i class="separator">&gt;</i>
            <span class="current" id="breadcrumb-current">${msg("registerTitle")}</span>
        </nav>
    <#elseif section = "header">
        <h2 class="portal-title">${msg("registerTitle")}</h2>
        <p class="portal-subtitle">${msg("registerSubtitle")} <a href="#" id="link-admin-contact" class="portal-link">${msg("requestToAdmins")}</a></p>
    <#elseif section = "form">
        <form id="kc-register-form" class="portal-form" action="${url.registrationAction}" method="post">
            <#if !realm.registrationEmailAsUsername>
                <input type="hidden" id="username" name="username" value="${(register.formData['username']!(register.formData['email']!''))}" />
            </#if>
            <input type="hidden" id="skin" name="skin" value="${(register.formData['skin']!'LIGHT')}" />

            <div class="portal-grid">
                <!-- Nome completo -->
                <div class="portal-form-group">
                    <div class="portal-input-wrapper <#if messagesPerField.existsError('firstName', 'lastName')>has-error</#if>">
                        <input type="text" id="firstName" name="firstName" value="${(register.formData['firstName']!'')}" placeholder=" " required />
                        <label for="firstName" class="portal-input-label">${msg("fullNameRequired")}</label>
                        <span class="material-icons portal-input-icon">input</span>
                    </div>
                    <#if messagesPerField.existsError('firstName')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('firstName'))?no_esc}</span>
                    <#elseif messagesPerField.existsError('lastName')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('lastName'))?no_esc}</span>
                    </#if>
                </div>

                <!-- Email -->
                <div class="portal-form-group">
                    <div class="portal-input-wrapper <#if messagesPerField.existsError('email', 'username')>has-error</#if>">
                        <input type="email" id="email" name="email" value="${(register.formData['email']!'')}" autocomplete="email" placeholder=" " required />
                        <label for="email" class="portal-input-label">${msg("emailRequired")}</label>
                        <span class="material-icons portal-input-icon">mail_outline</span>
                    </div>
                    <#if messagesPerField.existsError('email')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('email'))?no_esc}</span>
                    <#elseif messagesPerField.existsError('username')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('username'))?no_esc}</span>
                    </#if>
                </div>

                <!-- CPF -->
                <div class="portal-form-group">
                    <div class="portal-input-wrapper <#if messagesPerField.existsError('cpf', 'user.attributes.cpf')>has-error</#if>">
                        <input type="text" id="cpf" name="cpf" value="${(register.formData['cpf']!'')}" placeholder=" " required />
                        <label for="cpf" class="portal-input-label">${msg("cpfRequired")}</label>
                        <span class="material-icons portal-input-icon">badge</span>
                    </div>
                    <#if messagesPerField.existsError('cpf')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('cpf'))?no_esc}</span>
                    <#elseif messagesPerField.existsError('user.attributes.cpf')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('user.attributes.cpf'))?no_esc}</span>
                    </#if>
                </div>

                <!-- Celular -->
                <div class="portal-form-group">
                    <div class="portal-input-wrapper <#if messagesPerField.existsError('phoneNumber', 'user.attributes.phoneNumber')>has-error</#if>">
                        <input type="tel" id="phoneNumber" name="phoneNumber" value="${(register.formData['phoneNumber']!'')}" placeholder=" " required />
                        <label for="phoneNumber" class="portal-input-label">${msg("phoneRequired")}</label>
                        <span class="material-icons portal-input-icon">phone</span>
                    </div>
                    <#if messagesPerField.existsError('phoneNumber')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('phoneNumber'))?no_esc}</span>
                    <#elseif messagesPerField.existsError('user.attributes.phoneNumber')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('user.attributes.phoneNumber'))?no_esc}</span>
                    </#if>
                </div>

                <!-- Setor -->
                <div class="portal-form-group">
                    <div class="portal-input-wrapper <#if messagesPerField.existsError('department', 'user.attributes.department')>has-error</#if>">
                        <input type="text" id="department" name="department" value="${(register.formData['department']!'')}" placeholder=" " required />
                        <label for="department" class="portal-input-label">${msg("departmentRequired")}</label>
                        <span class="material-icons portal-input-icon">work_outline</span>
                    </div>
                    <#if messagesPerField.existsError('department')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('department'))?no_esc}</span>
                    <#elseif messagesPerField.existsError('user.attributes.department')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('user.attributes.department'))?no_esc}</span>
                    </#if>
                </div>

                <!-- Matrícula -->
                <div class="portal-form-group">
                    <div class="portal-input-wrapper <#if messagesPerField.existsError('registration', 'user.attributes.registration')>has-error</#if>">
                        <input type="text" id="registration" name="registration" value="${(register.formData['registration']!'')}" placeholder=" " required />
                        <label for="registration" class="portal-input-label">${msg("registrationRequired")}</label>
                        <span class="material-icons portal-input-icon">school</span>
                    </div>
                    <#if messagesPerField.existsError('registration')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('registration'))?no_esc}</span>
                    <#elseif messagesPerField.existsError('user.attributes.registration')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('user.attributes.registration'))?no_esc}</span>
                    </#if>
                </div>

                <!-- Senha -->
                <div class="portal-form-group">
                    <div class="portal-input-wrapper <#if messagesPerField.existsError('password')>has-error</#if>">
                        <input type="password" id="password" name="password" autocomplete="new-password" placeholder=" " required />
                        <label for="password" class="portal-input-label">${msg("passwordRequired")}</label>
                        <span class="material-icons portal-input-icon password-toggle" data-target="password">visibility</span>
                    </div>
                    <#if messagesPerField.existsError('password')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('password'))?no_esc}</span>
                    </#if>
                </div>

                <!-- Confirmar senha -->
                <div class="portal-form-group">
                    <div class="portal-input-wrapper <#if messagesPerField.existsError('password-confirm', 'passwordConfirm')>has-error</#if>">
                        <input type="password" id="password-confirm" name="password-confirm" autocomplete="new-password" placeholder=" " required />
                        <label for="password-confirm" class="portal-input-label">${msg("passwordConfirmRequired")}</label>
                        <span class="material-icons portal-input-icon password-toggle" data-target="password-confirm">visibility</span>
                    </div>
                    <#if messagesPerField.existsError('password-confirm')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('password-confirm'))?no_esc}</span>
                    <#elseif messagesPerField.existsError('passwordConfirm')>
                        <span class="portal-error-msg">${kcSanitize(messagesPerField.get('passwordConfirm'))?no_esc}</span>
                    </#if>
                </div>
            </div>

            <button class="portal-btn" type="submit">${msg("doRegisterUser")}</button>
            <p class="portal-footer-text">${msg("alreadyRegistered")} <a href="${url.loginUrl}" class="portal-link">${msg("loginLink")}</a></p>
        </form>

        <!-- Admin contact view -->
        <div id="portal-admin-box" class="portal-admin-box">
            <svg xmlns="http://www.w3.org/2000/svg" width="90" height="72" viewBox="0 0 113 90" fill="none">
                <path d="M90.4 50.625C92.0008 50.625 93.3436 50.085 94.4284 49.005C95.5132 47.925 96.0538 46.59 96.05 45C96.0462 43.41 95.5038 42.075 94.4228 40.995C93.3418 39.915 92.0008 39.375 90.4 39.375H73.45C71.8492 39.375 70.5082 39.915 69.4272 40.995C68.3462 42.075 67.8038 43.41 67.8 45C67.7962 46.59 68.3386 47.9269 69.4272 49.0106C70.5158 50.0944 71.8567 50.6325 73.45 50.625H90.4ZM90.4 33.75C92.0008 33.75 93.3436 33.21 94.4284 32.13C95.5132 31.05 96.0538 29.715 96.05 28.125C96.0462 26.535 95.5038 25.2 94.4228 24.12C93.3418 23.04 92.0008 22.5 90.4 22.5H73.45C71.8492 22.5 70.5082 23.04 69.4272 24.12C68.3462 25.2 67.8038 26.535 67.8 28.125C67.7962 29.715 68.3386 31.0519 69.4272 32.1356C70.5158 33.2194 71.8567 33.7575 73.45 33.75H90.4ZM39.55 50.625C36.16 50.625 33.0996 50.9306 30.3687 51.5419C27.6379 52.1531 25.2367 53.1131 23.165 54.4219C21.1875 55.6406 19.6808 57.0244 18.645 58.5731C17.6092 60.1219 17.0912 61.785 17.0912 63.5625C17.0912 64.6875 17.515 65.625 18.3625 66.375C19.21 67.125 20.2458 67.5 21.47 67.5H57.63C58.8542 67.5 59.89 67.1006 60.7375 66.3019C61.585 65.5031 62.0087 64.4962 62.0087 63.2812C62.0087 61.6875 61.4908 60.1406 60.455 58.6406C59.4192 57.1406 57.9125 55.7344 55.935 54.4219C53.8633 53.1094 51.4621 52.1475 48.7312 51.5362C46.0004 50.925 42.94 50.6212 39.55 50.625ZM39.55 45C42.6575 45 45.3168 43.8994 47.5278 41.6981C49.7388 39.4969 50.8462 36.8475 50.85 33.75C50.8538 30.6525 49.7482 28.005 47.5334 25.8075C45.3186 23.61 42.6575 22.5075 39.55 22.5C36.4425 22.4925 33.7832 23.595 31.5722 25.8075C29.3612 28.02 28.2538 30.6675 28.25 33.75C28.2462 36.8325 29.3536 39.4819 31.5722 41.6981C33.7908 43.9144 36.45 45.015 39.55 45ZM11.3 90C8.1925 90 5.53323 88.8994 3.3222 86.6981C1.11117 84.4969 0.00376667 81.8475 0 78.75V11.25C0 8.15625 1.1074 5.50875 3.3222 3.3075C5.537 1.10625 8.19627 0.00375 11.3 0H101.7C104.807 0 107.469 1.1025 109.683 3.3075C111.898 5.5125 113.004 8.16 113 11.25V78.75C113 81.8437 111.894 84.4931 109.683 86.6981C107.472 88.9031 104.811 90.0037 101.7 90H11.3Z" fill="#0578AC"/>
            </svg>
            <h3 class="portal-title">${msg("contactTitle")}</h3>
            <p class="portal-subtitle">${msg("contactDescription")}</p>
            <div class="portal-admin-info">
                <span><b>${msg("adminRoleLabel")}</b></span>
                <span><b>Nome:</b> ${msg("adminName")}</span>
                <span><b>Setor:</b> ${msg("adminSector")}</span>
                <span><b>Email:</b> ${msg("adminEmail")}</span>
            </div>
            <button type="button" id="btn-back-to-form" class="portal-btn">${msg("backToForm")}</button>
        </div>
    </#if>
</@layout.registrationLayout>
