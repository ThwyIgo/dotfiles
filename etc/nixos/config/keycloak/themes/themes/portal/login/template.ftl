<#macro registrationLayout bodyClass="" displayInfo=false displayMessage=true displayRequiredFields=false cardClass="">
<!DOCTYPE html>
<html lang="${lang!"pt-BR"}">
<head>
    <meta charset="utf-8">
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="robots" content="noindex, nofollow">
    <title>${msg("loginTitle",(realm.displayName!'Portal Physis'))}</title>
    <link rel="icon" href="${url.resourcesPath}/img/favicon.ico" />
    
    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
    <link href="https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;700&family=Lora:ital,wght@0,400..700;1,400..700&display=swap" rel="stylesheet">
    
    <link href="${url.resourcesPath}/css/styles.css" rel="stylesheet" />
    <#if properties.styles?has_content>
        <#list properties.styles?split(' ') as style>
            <link href="${url.resourcesPath}/${style}" rel="stylesheet" />
        </#list>
    </#if>
</head>

<body class="login-background ${bodyClass}">
    <div class="portal-container">
        <div class="portal-card ${cardClass}">
            <#if realm.internationalizationEnabled?? && realm.internationalizationEnabled && locale?? && locale.supported?? && locale.supported?size gt 1>
                <div class="portal-lang-selector">
                    <#list locale.supported as l>
                        <#assign langTag = (l.languageTag!'')>
                        <#if langTag?starts_with('pt')><#assign label = 'PT'><#elseif langTag?starts_with('en')><#assign label = 'EN'><#else><#assign label = (l.label!'')?substring(0, 2)?upper_case></#if>
                        <a href="${l.url}" class="portal-lang-link <#if (locale.currentLanguageTag!'') == langTag || (locale.current!'') == l.label>active</#if>">${label}</a>
                        <#sep><span class="portal-lang-sep">|</span></#sep>
                    </#list>
                </div>
            </#if>

            <#nested "breadcrumb">

            <#nested "owl">

            <#nested "header">

            <#if displayMessage && message?has_content && (message.type != 'warning' || !isAppInitiatedAction??)>
                <div class="portal-alert alert-${message.type}">
                    <span class="material-icons">
                        <#if message.type = 'success'>check_circle<#elseif message.type = 'error'>error_outline<#else>info</#if>
                    </span>
                    <span>${kcSanitize(message.summary)?no_esc}</span>
                </div>
            </#if>

            <#nested "form">

            <#if displayInfo>
                <div class="portal-footer-text">
                    <#nested "info">
                </div>
            </#if>
        </div>
    </div>

    <script src="${url.resourcesPath}/js/script.js"></script>
    <#if scripts??>
        <#list scripts as script>
            <script src="${script}" type="text/javascript"></script>
        </#list>
    </#if>
</body>
</html>
</#macro>
