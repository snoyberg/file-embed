## 0.1.0

* Injection has been moved to a submodule, "Data.FileEmbed.Inject".
* The ByteString-centric API has been shifted over to one that can produce any member of the IsString typeclass.
    * Correspondingly, the `embedString*` API has been removed, as it is now redundant.
*  Several generally-unused internal functions are no longer exported.

## 0.0.10

* `makeRelativeToProject`

## 0.0.9

* embedStringFile [#14](https://github.com/snoyberg/file-embed/pull/14)

## 0.0.8.2

* Improve inject documentation [#11](https://github.com/snoyberg/file-embed/issues/11)

## 0.0.8.1

Minor cleanup

## 0.0.8

* Custom postfix for magic string [#7](https://github.com/snoyberg/file-embed/issues/7)

## 0.0.7.1

Minor cleanup
