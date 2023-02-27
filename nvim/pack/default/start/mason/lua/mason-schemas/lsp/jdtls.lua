-- THIS FILE IS GENERATED. DO NOT EDIT MANUALLY.
-- stylua: ignore start
return {properties = {["java.autobuild.enabled"] = {default = true,description = "Enable/disable the 'auto build'",scope = "window",type = "boolean"},["java.cleanup.actionsOnSave"] = {default = {},items = {enum = { "qualifyMembers", "qualifyStaticMembers", "addOverride", "addDeprecated", "stringConcatToTextBlock", "invertEquals", "addFinalModifier", "instanceofPatternMatch", "lambdaExpression", "switchExpression", "tryWithResource" },type = "string"},markdownDescription = "The list of clean ups to be run on the current document when it's saved. Clean ups can automatically fix code style or programming mistakes. Click [HERE](command:_java.learnMoreAboutCleanUps) to learn more about what each clean up does.",scope = "window",type = "array"},["java.codeAction.sortMembers.avoidVolatileChanges"] = {default = true,description = "Reordering of fields, enum constants, and initializers can result in semantic and runtime changes due to different initialization and persistence order. This setting prevents this from occurring.",scope = "window",type = "boolean"},["java.codeGeneration.generateComments"] = {default = false,description = "Generate method comments when generating the methods.",scope = "window",type = "boolean"},["java.codeGeneration.hashCodeEquals.useInstanceof"] = {default = false,description = "Use 'instanceof' to compare types when generating the hashCode and equals methods.",scope = "window",type = "boolean"},["java.codeGeneration.hashCodeEquals.useJava7Objects"] = {default = false,description = "Use Objects.hash and Objects.equals when generating the hashCode and equals methods. This setting only applies to Java 7 and higher.",scope = "window",type = "boolean"},["java.codeGeneration.insertionLocation"] = {default = "afterCursor",description = "Specifies the insertion location of the code generated by source actions.",enum = { "afterCursor", "beforeCursor", "lastMember" },enumDescriptions = { "Insert the generated code after the member where the cursor is located.", "Insert the generated code before the member where the cursor is located.", "Insert the generated code as the last member of the target type." },scope = "window",type = "string"},["java.codeGeneration.toString.codeStyle"] = {default = "STRING_CONCATENATION",description = "The code style for generating the toString method.",enum = { "STRING_CONCATENATION", "STRING_BUILDER", "STRING_BUILDER_CHAINED", "STRING_FORMAT" },enumDescriptions = { "String concatenation", "StringBuilder/StringBuffer", "StringBuilder/StringBuffer - chained call", "String.format/MessageFormat" },type = "string"},["java.codeGeneration.toString.limitElements"] = {default = 0,description = "Limit number of items in arrays/collections/maps to list, if 0 then list all.",scope = "window",type = "integer"},["java.codeGeneration.toString.listArrayContents"] = {default = true,description = "List contents of arrays instead of using native toString().",scope = "window",type = "boolean"},["java.codeGeneration.toString.skipNullValues"] = {default = false,description = "Skip null values when generating the toString method.",scope = "window",type = "boolean"},["java.codeGeneration.toString.template"] = {default = "${object.className} [${member.name()}=${member.value}, ${otherMembers}]",description = "The template for generating the toString method.",type = "string"},["java.codeGeneration.useBlocks"] = {default = false,description = "Use blocks in 'if' statements when generating the methods.",scope = "window",type = "boolean"},["java.compile.nullAnalysis.mode"] = {default = "interactive",enum = { "disabled", "interactive", "automatic" },markdownDescription = "Specify how to enable the annotation-based null analysis.",scope = "window",type = "string"},["java.compile.nullAnalysis.nonnull"] = {default = { "javax.annotation.Nonnull", "org.eclipse.jdt.annotation.NonNull", "org.springframework.lang.NonNull" },markdownDescription = "Specify the Nonnull annotation types to be used for null analysis. If more than one annotation is specified, then the topmost annotation will be used first if it exists in project dependencies. This setting will be ignored if `java.compile.nullAnalysis.mode` is set to `disabled`",scope = "window",type = "array"},["java.compile.nullAnalysis.nullable"] = {default = { "javax.annotation.Nullable", "org.eclipse.jdt.annotation.Nullable", "org.springframework.lang.Nullable" },markdownDescription = "Specify the Nullable annotation types to be used for null analysis. If more than one annotation is specified, then the topmost annotation will be used first if it exists in project dependencies. This setting will be ignored if `java.compile.nullAnalysis.mode` is set to `disabled`",scope = "window",type = "array"},["java.completion.enabled"] = {default = true,description = "Enable/disable code completion support",scope = "window",type = "boolean"},["java.completion.favoriteStaticMembers"] = {default = { "org.junit.Assert.*", "org.junit.Assume.*", "org.junit.jupiter.api.Assertions.*", "org.junit.jupiter.api.Assumptions.*", "org.junit.jupiter.api.DynamicContainer.*", "org.junit.jupiter.api.DynamicTest.*", "org.mockito.Mockito.*", "org.mockito.ArgumentMatchers.*", "org.mockito.Answers.*" },description = "Defines a list of static members or types with static members. Content assist will propose those static members even if the import is missing.",scope = "window",type = "array"},["java.completion.filteredTypes"] = {default = { "java.awt.*", "com.sun.*", "sun.*", "jdk.*", "org.graalvm.*", "io.micrometer.shaded.*" },description = "Defines the type filters. All types whose fully qualified name matches the selected filter strings will be ignored in content assist or quick fix proposals and when organizing imports. For example 'java.awt.*' will hide all types from the awt packages.",scope = "window",type = "array"},["java.completion.guessMethodArguments"] = {default = true,description = "When set to true, method arguments are guessed when a method is selected from as list of code assist proposals.",scope = "window",type = "boolean"},["java.completion.importOrder"] = {default = { "#", "java", "javax", "org", "com", "" },description = "Defines the sorting order of import statements. A package or type name prefix (e.g. 'org.eclipse') is a valid entry. An import is always added to the most specific group. As a result, the empty string (e.g. '') can be used to group all other imports. Static imports are prefixed with a '#'",scope = "window",type = "array"},["java.completion.matchCase"] = {default = "auto",enum = { "auto", "firstLetter", "off" },enumDescriptions = { "Only match case for the first letter when using Visual Studio Code - Insiders.", "Match case for the first letter when doing completion.", "Do not match case when doing completion." },markdownDescription = "Specify whether to match case for code completion.",scope = "window",type = "string"},["java.completion.maxResults"] = {default = 0,markdownDescription = "Maximum number of completion results (not including snippets).\n`0` (the default value) disables the limit, all results are returned. In case of performance problems, consider setting a sensible limit.",scope = "window",type = "integer"},["java.completion.postfix.enabled"] = {default = true,markdownDescription = "Enable/disable postfix completion support. `#editor.snippetSuggestions#` can be used to customize how postfix snippets are sorted.",scope = "window",type = "boolean"},["java.configuration.checkProjectSettingsExclusions"] = {default = false,deprecationMessage = "Please use 'java.import.generatesMetadataFilesAtProjectRoot' to control whether to generate the project metadata files at the project root. And use 'files.exclude' to control whether to hide the project metadata files from the file explorer.",description = "Controls whether to exclude extension-generated project settings files (.project, .classpath, .factorypath, .settings/) from the file explorer.",scope = "window",type = "boolean"},["java.configuration.maven.defaultMojoExecutionAction"] = {default = "ignore",description = "Specifies default mojo execution action when no associated metadata can be detected.",enum = { "ignore", "warn", "error", "execute" },scope = "window",type = "string"},["java.configuration.maven.globalSettings"] = {default = vim.NIL,description = "Path to Maven's global settings.xml",scope = "window",type = "string"},["java.configuration.maven.notCoveredPluginExecutionSeverity"] = {default = "warning",description = "Specifies severity if the plugin execution is not covered by Maven build lifecycle.",enum = { "ignore", "warning", "error" },scope = "window",type = "string"},["java.configuration.maven.userSettings"] = {default = vim.NIL,description = "Path to Maven's user settings.xml",scope = "window",type = "string"},["java.configuration.runtimes"] = {default = {},description = "Map Java Execution Environments to local JDKs.",items = {additionalProperties = false,default = vim.empty_dict(),properties = {default = {description = "Is default runtime? Only one runtime can be default.",type = "boolean"},javadoc = {description = "JDK javadoc path.",type = "string"},name = {description = "Java Execution Environment name. Must be unique.",enum = { "J2SE-1.5", "JavaSE-1.6", "JavaSE-1.7", "JavaSE-1.8", "JavaSE-9", "JavaSE-10", "JavaSE-11", "JavaSE-12", "JavaSE-13", "JavaSE-14", "JavaSE-15", "JavaSE-16", "JavaSE-17", "JavaSE-18", "JavaSE-19" },type = "string"},path = {description = 'JDK home path. Should be the JDK installation directory, not the Java bin path.\n On Windows, backslashes must be escaped, i.e.\n"path":"C:\\\\Program Files\\\\Java\\\\jdk1.8.0_161".',pattern = ".*(?<!\\/bin|\\/bin\\/|\\\\bin|\\\\bin\\\\)$",type = "string"},sources = {description = "JDK sources path.",type = "string"}},required = { "path", "name" },type = "object"},scope = "machine-overridable",type = "array"},["java.configuration.updateBuildConfiguration"] = {default = "interactive",description = "Specifies how modifications on build files update the Java classpath/configuration",enum = { "disabled", "interactive", "automatic" },scope = "window",type = { "string" }},["java.configuration.workspaceCacheLimit"] = {default = 90,description = "The number of days (if enabled) to keep unused workspace cache data. Beyond this limit, cached workspace data may be removed.",minimum = 1,scope = "application",type = { "null", "integer" }},["java.contentProvider.preferred"] = {default = vim.NIL,description = "Preferred content provider (a 3rd party decompiler id, usually)",scope = "window",type = "string"},["java.eclipse.downloadSources"] = {default = false,description = "Enable/disable download of Maven source artifacts for Eclipse projects.",scope = "window",type = "boolean"},["java.errors.incompleteClasspath.severity"] = {default = "warning",description = "Specifies the severity of the message when the classpath is incomplete for a Java file",enum = { "ignore", "info", "warning", "error" },scope = "window",type = { "string" }},["java.foldingRange.enabled"] = {default = true,description = "Enable/disable smart folding range support. If disabled, it will use the default indentation-based folding range provided by VS Code.",scope = "window",type = "boolean"},["java.format.comments.enabled"] = {default = true,description = "Includes the comments during code formatting.",scope = "window",type = "boolean"},["java.format.enabled"] = {default = true,description = "Enable/disable default Java formatter",scope = "window",type = "boolean"},["java.format.onType.enabled"] = {default = true,description = "Enable/disable automatic block formatting when typing `;`, `<enter>` or `}`",scope = "window",type = "boolean"},["java.format.settings.profile"] = {default = vim.NIL,description = "Optional formatter profile name from the Eclipse formatter settings.",scope = "window",type = "string"},["java.format.settings.url"] = {default = vim.NIL,markdownDescription = "Specifies the url or file path to the [Eclipse formatter xml settings](https://github.com/redhat-developer/vscode-java/wiki/Formatter-settings).",scope = "window",type = "string"},["java.home"] = {default = vim.NIL,deprecationMessage = "This setting is deprecated, please use 'java.jdt.ls.java.home' instead.",description = 'Specifies the folder path to the JDK (17 or more recent) used to launch the Java Language Server.\nOn Windows, backslashes must be escaped, i.e.\n"java.home":"C:\\\\Program Files\\\\Java\\\\jdk-17.0_3"',scope = "machine-overridable",type = { "string", "null" }},["java.implementationsCodeLens.enabled"] = {default = false,description = "Enable/disable the implementations code lens.",scope = "window",type = "boolean"},["java.import.exclusions"] = {default = { "**/node_modules/**", "**/.metadata/**", "**/archetype-resources/**", "**/META-INF/maven/**" },description = "Configure glob patterns for excluding folders. Use `!` to negate patterns to allow subfolders imports. You have to include a parent directory. The order is important.",scope = "window",type = "array"},["java.import.generatesMetadataFilesAtProjectRoot"] = {default = false,markdownDescription = "Specify whether the project metadata files(.project, .classpath, .factorypath, .settings/) will be generated at the project root. Click [HERE](command:_java.metadataFilesGeneration) to learn how to change the setting to make it take effect.",scope = "window",type = "boolean"},["java.import.gradle.annotationProcessing.enabled"] = {default = true,description = "Enable/disable the annotation processing on Gradle projects and delegate Annotation Processing to JDT APT. Only works for Gradle 5.2 or higher.",scope = "window",type = "boolean"},["java.import.gradle.arguments"] = {default = vim.NIL,description = "Arguments to pass to Gradle.",scope = "machine",type = "string"},["java.import.gradle.enabled"] = {default = true,description = "Enable/disable the Gradle importer.",scope = "window",type = "boolean"},["java.import.gradle.home"] = {default = vim.NIL,description = "Use Gradle from the specified local installation directory or GRADLE_HOME if the Gradle wrapper is missing or disabled and no 'java.import.gradle.version' is specified.",scope = "window",type = "string"},["java.import.gradle.java.home"] = {default = vim.NIL,description = "The location to the JVM used to run the Gradle daemon.",scope = "machine-overridable",type = "string"},["java.import.gradle.jvmArguments"] = {default = vim.NIL,description = "JVM arguments to pass to Gradle.",scope = "machine",type = "string"},["java.import.gradle.offline.enabled"] = {default = false,description = "Enable/disable the Gradle offline mode.",scope = "window",type = "boolean"},["java.import.gradle.user.home"] = {default = vim.NIL,description = "Setting for GRADLE_USER_HOME.",scope = "window",type = "string"},["java.import.gradle.version"] = {default = vim.NIL,description = "Use Gradle from the specific version if the Gradle wrapper is missing or disabled.",scope = "window",type = "string"},["java.import.gradle.wrapper.enabled"] = {default = true,description = "Use Gradle from the 'gradle-wrapper.properties' file.",scope = "window",type = "boolean"},["java.import.maven.disableTestClasspathFlag"] = {default = false,description = "Enable/disable test classpath segregation. When enabled, this permits the usage of test resources within a Maven project as dependencies within the compile scope of other projects.",scope = "window",type = "boolean"},["java.import.maven.enabled"] = {default = true,description = "Enable/disable the Maven importer.",scope = "window",type = "boolean"},["java.import.maven.offline.enabled"] = {default = false,description = "Enable/disable the Maven offline mode.",scope = "window",type = "boolean"},["java.imports.gradle.wrapper.checksums"] = {default = {},description = "Defines allowed/disallowed SHA-256 checksums of Gradle Wrappers",items = {additionalProperties = false,default = vim.empty_dict(),properties = {allowed = {default = true,label = "Is allowed?",type = "boolean"},sha256 = {label = "SHA-256 checksum.",type = "string"}},required = { "sha256" },type = "object",uniqueItems = true},scope = "application",type = "array"},["java.inlayHints.parameterNames.enabled"] = {default = "literals",enum = { "none", "literals", "all" },enumDescriptions = { "Disable parameter name hints", "Enable parameter name hints only for literal arguments", "Enable parameter name hints for literal and non-literal arguments" },markdownDescription = "Enable/disable inlay hints for parameter names:\n```java\n\nInteger.valueOf(/* s: */ '123', /* radix: */ 10)\n \n```\n `#java.inlayHints.parameterNames.exclusions#` can be used to disable the inlay hints for methods.",scope = "window",type = "string"},["java.inlayHints.parameterNames.exclusions"] = {default = {},items = {type = "string"},markdownDescription = "The patterns for the methods that will be disabled to show the inlay hints. Supported pattern examples:\n - `java.lang.Math.*` - All the methods from java.lang.Math.\n - `*.Arrays.asList` - Methods named as 'asList' in the types named as 'Arrays'.\n - `*.println(*)` - Methods named as 'println'.\n - `(from, to)` - Methods with two parameters named as 'from' and 'to'.\n - `(arg*)` - Methods with one parameter whose name starts with 'arg'.",scope = "window",type = "array"},["java.jdt.ls.androidSupport.enabled"] = {default = "auto",enum = { "auto", "on", "off" },markdownDescription = "[Experimental] Specify whether to enable Android project importing. When set to `auto`, the Android support will be enabled in Visual Studio Code - Insiders.\n\n**Note:** Only works for Android Gradle Plugin `3.2.0` or higher.",scope = "window",type = "string"},["java.jdt.ls.java.home"] = {default = vim.NIL,description = "Specifies the folder path to the JDK (17 or more recent) used to launch the Java Language Server. This setting will replace the Java extension's embedded JRE to start the Java Language Server. \n\nOn Windows, backslashes must be escaped, i.e.\n\"java.jdt.ls.java.home\":\"C:\\\\Program Files\\\\Java\\\\jdk-17.0_3\"",scope = "machine-overridable",type = { "string", "null" }},["java.jdt.ls.lombokSupport.enabled"] = {default = true,description = "Whether to load lombok processors from project classpath",scope = "window",type = "boolean"},["java.jdt.ls.protobufSupport.enabled"] = {default = true,markdownDescription = "Specify whether to automatically add Protobuf output source directories to the classpath.\n\n**Note:** Only works for Gradle `com.google.protobuf` plugin `0.8.4` or higher.",scope = "window",type = "boolean"},["java.jdt.ls.vmargs"] = {default = "-XX:+UseParallelGC -XX:GCTimeRatio=4 -XX:AdaptiveSizePolicyWeight=90 -Dsun.zip.disableMemoryMapping=true -Xmx1G -Xms100m -Xlog:disable",description = "Specifies extra VM arguments used to launch the Java Language Server. Eg. use `-XX:+UseParallelGC -XX:GCTimeRatio=4 -XX:AdaptiveSizePolicyWeight=90 -Dsun.zip.disableMemoryMapping=true -Xmx1G -Xms100m -Xlog:disable` to optimize memory usage with the parallel garbage collector",scope = "machine-overridable",type = { "string", "null" }},["java.maven.downloadSources"] = {default = false,description = "Enable/disable download of Maven source artifacts as part of importing Maven projects.",scope = "window",type = "boolean"},["java.maven.updateSnapshots"] = {default = false,description = "Force update of Snapshots/Releases.",scope = "window",type = "boolean"},["java.maxConcurrentBuilds"] = {default = 1,description = "Max simultaneous project builds",minimum = 1,scope = "window",type = "integer"},["java.progressReports.enabled"] = {default = true,description = "[Experimental] Enable/disable progress reports from background processes on the server.",scope = "window",type = "boolean"},["java.project.encoding"] = {default = "ignore",enum = { "ignore", "warning", "setDefault" },enumDescriptions = { "Ignore project encoding settings", "Show warning if a project has no explicit encoding set", "Set the default workspace encoding settings" },markdownDescription = "Project encoding settings",scope = "window"},["java.project.importHint"] = {default = true,description = "Enable/disable the server-mode switch information, when Java projects import is skipped on startup.",scope = "application",type = "boolean"},["java.project.importOnFirstTimeStartup"] = {default = "automatic",description = "Specifies whether to import the Java projects, when opening the folder in Hybrid mode for the first time.",enum = { "disabled", "interactive", "automatic" },scope = "application",type = "string"},["java.project.outputPath"] = {default = "",markdownDescription = "A relative path to the workspace where stores the compiled output. `Only` effective in the `WORKSPACE` scope. The setting will `NOT` affect Maven or Gradle project.",scope = "window",type = { "string", "null" }},["java.project.referencedLibraries"] = {additionalProperties = false,default = { "lib/**/*.jar" },description = "Configure glob patterns for referencing local libraries to a Java project.",properties = {exclude = {type = "array"},include = {type = "array"},sources = {type = "object"}},required = { "include" },scope = "window",type = { "array", "object" }},["java.project.resourceFilters"] = {default = { "node_modules", "\\.git" },description = "Excludes files and folders from being refreshed by the Java Language Server, which can improve the overall performance. For example, [\"node_modules\",\"\\.git\"] will exclude all files and folders named 'node_modules' or '.git'. Pattern expressions must be compatible with `java.util.regex.Pattern`. Defaults to [\"node_modules\",\"\\.git\"].",scope = "window",type = "array"},["java.project.sourcePaths"] = {default = {},items = {type = "string"},markdownDescription = "Relative paths to the workspace where stores the source files. `Only` effective in the `WORKSPACE` scope. The setting will `NOT` affect Maven or Gradle project.",scope = "window",type = "array"},["java.quickfix.showAt"] = {default = "line",description = "Show quickfixes at the problem or line level.",enum = { "line", "problem" },scope = "window",type = "string"},["java.recommendations.dependency.analytics.show"] = {default = true,description = "Show the recommended Dependency Analytics extension.",scope = "window",type = "boolean"},["java.refactoring.extract.interface.replace"] = {default = true,markdownDescription = "Specify whether to replace all the occurrences of the subtype with the new extracted interface.",type = "boolean"},["java.references.includeAccessors"] = {default = true,description = "Include getter, setter and builder/constructor when finding references.",scope = "window",type = "boolean"},["java.references.includeDecompiledSources"] = {default = true,description = "Include the decompiled sources when finding references.",scope = "window",type = "boolean"},["java.referencesCodeLens.enabled"] = {default = false,description = "Enable/disable the references code lens.",scope = "window",type = "boolean"},["java.saveActions.organizeImports"] = {default = false,description = "Enable/disable auto organize imports on save action",scope = "window",type = "boolean"},["java.selectionRange.enabled"] = {default = true,description = "Enable/disable Smart Selection support for Java. Disabling this option will not affect the VS Code built-in word-based and bracket-based smart selection.",scope = "window",type = "boolean"},["java.server.launchMode"] = {default = "Hybrid",description = "The launch mode for the Java extension",enum = { "Standard", "LightWeight", "Hybrid" },enumDescriptions = { "Provides full features such as intellisense, refactoring, building, Maven/Gradle support etc.", "Starts a syntax server with lower start-up cost. Only provides syntax features such as outline, navigation, javadoc, syntax errors.", "Provides full features with better responsiveness. It starts a standard language server and a secondary syntax server. The syntax server provides syntax features until the standard server is ready." },scope = "window",type = "string"},["java.settings.url"] = {default = vim.NIL,markdownDescription = "Specifies the url or file path to the workspace Java settings. See [Setting Global Preferences](https://github.com/redhat-developer/vscode-java/wiki/Settings-Global-Preferences)",scope = "window",type = "string"},["java.sharedIndexes.enabled"] = {default = "auto",enum = { "auto", "on", "off" },markdownDescription = "[Experimental] Specify whether to share indexes between different workspaces. When set to `auto`, shared indexes will be enabled in Visual Studio Code - Insiders.",scope = "window",type = "string"},["java.sharedIndexes.location"] = {default = "",markdownDescription = 'Specifies a common index location for all workspaces. See default values as follows:\n \nWindows: First use `"$APPDATA\\\\.jdt\\\\index"`, or `"~\\\\.jdt\\\\index"` if it does not exist\n \nmacOS: `"~/Library/Caches/.jdt/index"`\n \nLinux: First use `"$XDG_CACHE_HOME/.jdt/index"`, or `"~/.cache/.jdt/index"` if it does not exist',scope = "window",type = "string"},["java.showBuildStatusOnStart.enabled"] = {anyOf = { {enum = { "notification", "terminal", "off" },enumDescriptions = { "Show the build status via progress notification on start", "Show the build status via terminal on start", "Do not show any build status on start" }}, "boolean" },default = "notification",description = "Automatically show build status on startup.",scope = "window"},["java.signatureHelp.description.enabled"] = {default = false,description = "Enable/disable to show the description in signature help.",scope = "window",type = "boolean"},["java.signatureHelp.enabled"] = {default = true,description = "Enable/disable the signature help.",scope = "window",type = "boolean"},["java.sources.organizeImports.starThreshold"] = {default = 99,description = "Specifies the number of imports added before a star-import declaration is used.",minimum = 1,scope = "window",type = "integer"},["java.sources.organizeImports.staticStarThreshold"] = {default = 99,description = "Specifies the number of static imports added before a star-import declaration is used.",minimum = 1,scope = "window",type = "integer"},["java.symbols.includeSourceMethodDeclarations"] = {default = false,markdownDescription = "Include method declarations from source files in symbol search.",scope = "window",type = "boolean"},["java.templates.fileHeader"] = {default = {},markdownDescription = "Specifies the file header comment for new Java file. Supports configuring multi-line comments with an array of strings, and using ${variable} to reference the [predefined variables](command:_java.templateVariables).",scope = "window",type = "array"},["java.templates.typeComment"] = {default = {},markdownDescription = "Specifies the type comment for new Java type. Supports configuring multi-line comments with an array of strings, and using ${variable} to reference the [predefined variables](command:_java.templateVariables).",scope = "window",type = "array"},["java.trace.server"] = {default = "off",description = "Traces the communication between VS Code and the Java language server.",enum = { "off", "messages", "verbose" },scope = "window",type = "string"},["java.typeHierarchy.lazyLoad"] = {default = false,description = "Enable/disable lazy loading the content in type hierarchy. Lazy loading could save a lot of loading time but every type should be expanded manually to load its content.",scope = "window",type = "boolean"}},title = "Java",type = "object"}