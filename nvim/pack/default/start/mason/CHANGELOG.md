# Changelog

## [1.8.3](https://github.com/williamboman/mason.nvim/compare/v1.8.2...v1.8.3) (2023-11-08)


### Bug Fixes

* **pypi:** support MSYS2 virtual environments on Windows ([#1547](https://github.com/williamboman/mason.nvim/issues/1547)) ([3e2432a](https://github.com/williamboman/mason.nvim/commit/3e2432ad0bca01fc3356389b341aa3e5e2da9fd8))

## [1.8.2](https://github.com/williamboman/mason.nvim/compare/v1.8.1...v1.8.2) (2023-10-31)


### Bug Fixes

* **registry:** fix parsing registry identifiers that contain ":" ([#1542](https://github.com/williamboman/mason.nvim/issues/1542)) ([87eb3ac](https://github.com/williamboman/mason.nvim/commit/87eb3ac2ab4fcbf5326d8bde6842b073a3be65a7))

## [1.8.1](https://github.com/williamboman/mason.nvim/compare/v1.8.0...v1.8.1) (2023-10-10)


### Bug Fixes

* **health:** schedule vim.fn call ([#1514](https://github.com/williamboman/mason.nvim/issues/1514)) ([3ba3b79](https://github.com/williamboman/mason.nvim/commit/3ba3b79f73d5411e72c7df5445150f4e9278d4d7))

## [1.8.0](https://github.com/williamboman/mason.nvim/compare/v1.7.0...v1.8.0) (2023-09-04)


### Features

* **ui:** add setting to toggle help view ([#1468](https://github.com/williamboman/mason.nvim/issues/1468)) ([e1602c8](https://github.com/williamboman/mason.nvim/commit/e1602c868f938877057cb6f45e50859cb55cad96))


### Bug Fixes

* **registry:** reset registries state when setting registries ([#1474](https://github.com/williamboman/mason.nvim/issues/1474)) ([c811fbf](https://github.com/williamboman/mason.nvim/commit/c811fbf09c7642eebb37d6694f1a016a043f6ed3))
* **registry:** schedule vim.fn calls in FileRegistrySource ([#1471](https://github.com/williamboman/mason.nvim/issues/1471)) ([1c77412](https://github.com/williamboman/mason.nvim/commit/1c77412d7ff73e453cdc5366c8d7cd98d2242802))

## [1.7.0](https://github.com/williamboman/mason.nvim/compare/v1.6.2...v1.7.0) (2023-08-25)


### Features

* **cargo:** support fetching versions for git crates hosted on github ([#1459](https://github.com/williamboman/mason.nvim/issues/1459)) ([e9eb004](https://github.com/williamboman/mason.nvim/commit/e9eb0048cecc577a1eec534485d3e010487b46a7))
* **registry:** add file: source protocol ([#1457](https://github.com/williamboman/mason.nvim/issues/1457)) ([8544039](https://github.com/williamboman/mason.nvim/commit/85440397264a31208721e4501c93b23a4940b27e))


### Bug Fixes

* **std:** use gtar if available ([#1433](https://github.com/williamboman/mason.nvim/issues/1433)) ([a51c2d0](https://github.com/williamboman/mason.nvim/commit/a51c2d063c5377ee9e58c5f9cda7c7436787be72))
* **ui:** properly reset new package version state ([#1454](https://github.com/williamboman/mason.nvim/issues/1454)) ([68e6a15](https://github.com/williamboman/mason.nvim/commit/68e6a153d7cd1251eb85ebb48d2e351e9ab940b8))

## [1.6.2](https://github.com/williamboman/mason.nvim/compare/v1.6.1...v1.6.2) (2023-08-09)


### Bug Fixes

* **ui:** don't disable search mode if empty pattern and last-pattern is set ([#1445](https://github.com/williamboman/mason.nvim/issues/1445)) ([be6f680](https://github.com/williamboman/mason.nvim/commit/be6f680774a75a06ceede3bd7159df2388f49b04))

## [1.6.1](https://github.com/williamboman/mason.nvim/compare/v1.6.0...v1.6.1) (2023-07-21)


### Bug Fixes

* **installer:** retain unmapped source fields ([#1399](https://github.com/williamboman/mason.nvim/issues/1399)) ([0579574](https://github.com/williamboman/mason.nvim/commit/05795741895ee16062eabeb0d89bff7cbcd693fa))

## [1.6.0](https://github.com/williamboman/mason.nvim/compare/v1.5.1...v1.6.0) (2023-07-04)


### Features

* **ui:** display package deprecation message ([#1391](https://github.com/williamboman/mason.nvim/issues/1391)) ([b728115](https://github.com/williamboman/mason.nvim/commit/b7281153cd9167d2b1a5d8cbda1ba8d4ad9fa8c2))
* **ui:** don't use diagnostic messages for displaying deprecated, uninstalled, packages ([#1393](https://github.com/williamboman/mason.nvim/issues/1393)) ([c290d0e](https://github.com/williamboman/mason.nvim/commit/c290d0e4ab6da9cac1e26684e53fba0b615862ed))

## [1.5.1](https://github.com/williamboman/mason.nvim/compare/v1.5.0...v1.5.1) (2023-06-28)


### Bug Fixes

* **linker:** ensure exec wrapper target is executable ([#1380](https://github.com/williamboman/mason.nvim/issues/1380)) ([10da1a3](https://github.com/williamboman/mason.nvim/commit/10da1a33b4ac24ad4d76a9af91871720ac6b65e4))
* **purl:** percent-encoding is case insensitive ([#1382](https://github.com/williamboman/mason.nvim/issues/1382)) ([b68d3be](https://github.com/williamboman/mason.nvim/commit/b68d3be4b664671002221d43c82e74a0f1006b26))

## [1.5.0](https://github.com/williamboman/mason.nvim/compare/v1.4.0...v1.5.0) (2023-06-28)


### Features

* **command:** add completion for option flags for :MasonInstall ([#1379](https://github.com/williamboman/mason.nvim/issues/1379)) ([e507af7](https://github.com/williamboman/mason.nvim/commit/e507af7b996dae90404345abb2bc88540f931589))
* **installer:** write more installation output to stdout ([#1376](https://github.com/williamboman/mason.nvim/issues/1376)) ([758ac5b](https://github.com/williamboman/mason.nvim/commit/758ac5b35e823eee74a90f855b2a66afc51ec92d))


### Bug Fixes

* **installer:** timeout schema download after 5s ([#1374](https://github.com/williamboman/mason.nvim/issues/1374)) ([d114376](https://github.com/williamboman/mason.nvim/commit/d11437645af60449ff252b2c9abda103c5610520))

## [1.4.0](https://github.com/williamboman/mason.nvim/compare/v1.3.0...v1.4.0) (2023-06-21)


### Features

* **fetch:** add explicit default timeout to requests ([#1364](https://github.com/williamboman/mason.nvim/issues/1364)) ([82cae55](https://github.com/williamboman/mason.nvim/commit/82cae550c87466b1163b216bdb9c71cb71dd8f67))
* **fetch:** include mason.nvim version in User-Agent ([#1362](https://github.com/williamboman/mason.nvim/issues/1362)) ([e706d30](https://github.com/williamboman/mason.nvim/commit/e706d305fbcc8701bd30e31dd727aee2853b9db9))

## [1.3.0](https://github.com/williamboman/mason.nvim/compare/v1.2.1...v1.3.0) (2023-06-18)


### Features

* **health:** add advice for Debian/Ubuntu regarding python3 venv ([#1358](https://github.com/williamboman/mason.nvim/issues/1358)) ([6f3853e](https://github.com/williamboman/mason.nvim/commit/6f3853e5ae8c200e29d2e394e479d9c3f8e018f5))

## [1.2.1](https://github.com/williamboman/mason.nvim/compare/v1.2.0...v1.2.1) (2023-06-13)


### Bug Fixes

* **providers:** fix some client providers and add some more ([#1354](https://github.com/williamboman/mason.nvim/issues/1354)) ([6f44955](https://github.com/williamboman/mason.nvim/commit/6f4495590a0f9e121b483c9b1236fbabbd80da7a))

## [1.2.0](https://github.com/williamboman/mason.nvim/compare/v1.1.1...v1.2.0) (2023-06-13)


### Features

* **command:** improve completion for :MasonInstall ([#1353](https://github.com/williamboman/mason.nvim/issues/1353)) ([13e26c8](https://github.com/williamboman/mason.nvim/commit/13e26c81ff5074ee8f095a791cd37fc1cec37377))


### Bug Fixes

* **async:** always check channel state ([#1351](https://github.com/williamboman/mason.nvim/issues/1351)) ([f503346](https://github.com/williamboman/mason.nvim/commit/f5033463bb911a136e577fc6f339328f162e2b4a))
* **command:** run :MasonUpdate synchronously in headless mode ([#1347](https://github.com/williamboman/mason.nvim/issues/1347)) ([0276793](https://github.com/williamboman/mason.nvim/commit/02767937fc2e1b214c854a8fdde26ae1d3529dd6))
* **functional:** strip_prefix and strip_suffix should not use patterns ([#1352](https://github.com/williamboman/mason.nvim/issues/1352)) ([f99b702](https://github.com/williamboman/mason.nvim/commit/f99b70233e49db2229350bb82d9ddc6e2f4131c0))

## [1.1.1](https://github.com/williamboman/mason.nvim/compare/v1.1.0...v1.1.1) (2023-05-29)


### Bug Fixes

* **ui:** improve search mode UI and remove redundant whitespaces ([#1332](https://github.com/williamboman/mason.nvim/issues/1332)) ([a18c031](https://github.com/williamboman/mason.nvim/commit/a18c031c72a3c7576ba5dc60ee30de8290c8757c))

## [1.1.0](https://github.com/williamboman/mason.nvim/compare/v1.0.1...v1.1.0) (2023-05-18)


### Features

* **installer:** lock package installation ([#1290](https://github.com/williamboman/mason.nvim/issues/1290)) ([227f8a9](https://github.com/williamboman/mason.nvim/commit/227f8a9aaae495f481c768f8346edfceaf6d2951))
* **ui:** add keymap setting for toggling package installation log ([#1268](https://github.com/williamboman/mason.nvim/issues/1268)) ([48bb1cc](https://github.com/williamboman/mason.nvim/commit/48bb1cc33a1fefe94f5ce4972446a1c6ad849f15))
* **ui:** add search mode ([#1306](https://github.com/williamboman/mason.nvim/issues/1306)) ([3b59f25](https://github.com/williamboman/mason.nvim/commit/3b59f25d435fb1b8d36c4cc26410c3569f0bd795))
* **ui:** display "update all" hint ([#1296](https://github.com/williamboman/mason.nvim/issues/1296)) ([e634134](https://github.com/williamboman/mason.nvim/commit/e634134312bb936f472468a401c9cae6485ab54b))


### Bug Fixes

* **sources:** don't skip installation if fixed version is not currently installed ([#1297](https://github.com/williamboman/mason.nvim/issues/1297)) ([9c5edf1](https://github.com/williamboman/mason.nvim/commit/9c5edf13c2e6bd5223eebfeb4557ccc841acaa0e))
* **ui:** use vim.cmd("") for nvim-0.7.0 compatibility ([#1307](https://github.com/williamboman/mason.nvim/issues/1307)) ([e60b855](https://github.com/williamboman/mason.nvim/commit/e60b855bfa8c7d34387200daa6e54a5e22d3da05))

## [1.0.1](https://github.com/williamboman/mason.nvim/compare/v1.0.0...v1.0.1) (2023-04-26)


### Bug Fixes

* **pypi:** also provide install_extra_args to pypi.install ([#1263](https://github.com/williamboman/mason.nvim/issues/1263)) ([646ef07](https://github.com/williamboman/mason.nvim/commit/646ef07907e0960987c13c0b13f69eb808cc66ad))
