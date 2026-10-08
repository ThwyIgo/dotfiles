;;; Directory Local Variables            -*- no-byte-compile: t -*-

((nil . ((compile-command . "dotnet run --project foo/bar.csproj")
         (compilation-environment . ("ASPNETCORE_ENVIRONMENT=Development"
                                     "ConnectionStrings__DefaultConnection=Server=localhost;Port=5432;Database=foo;User Id=postgres;Password=1234;"))
         (eval . (let ((root (locate-dominating-file default-directory ".dir-locals.el")))
                   (add-to-list 'dape-configs
                                `(debug-foo
                                  modes (csharp-mode csharp-ts-mode)
                                  ensure dape-ensure-command
                                  command "netcoredbg"
                                  command-args ["--interpreter=vscode"]
                                  :request "launch"
                                  :compile "dotnet build foo/bar.csproj -p:Configuration=Debug -p:TargetFramework=net10.0"
                                  :cwd ,(expand-file-name "foo/bin/Debug/net10.0/" root)
                                  :program ,(expand-file-name "foo/bin/Debug/net10.0/bar.dll" root)
                                  :env (:ASPNETCORE_ENVIRONMENT "Development"
                                                                :ConnectionStrings__DefaultConnection "Server=localhost;Port=5432;Database=foo;User Id=postgres;Password=1234;")))
                   (add-to-list 'dape-configs
                                `(debug-foo-dev
                                  modes (csharp-mode csharp-ts-mode)
                                  ensure dape-ensure-command
                                  command "netcoredbg"
                                  command-args ["--interpreter=vscode"]
                                  :request "launch"
                                  :compile "dotnet build foo/bar.csproj -p:Configuration=Debug -p:TargetFramework=net10.0"
                                  :cwd ,(expand-file-name "foo/bin/Debug/net10.0/" root)
                                  :program ,(expand-file-name "foo/bin/Debug/net10.0/bar.dll" root)
                                  :env (:ASPNETCORE_ENVIRONMENT "Development"
                                                                :ConnectionStrings__DefaultConnection "Server=192.168.1.0;Port=5432;User Id=postgres;Password=4321;Database=foo"))))))))
