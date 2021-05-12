# gm82save
This is a hack that allows GameMaker 8.1 to save to a custom .gm82 format, which plays nicer with git. There's a few things you should be aware of before working with this.

* This **may still contain bugs**, so keep a backup of your .gm81 if you're converting a project to this. Let me know about any bugs you find.
* If saving fails partway through, **your files may be inconsistent**. It shouldn't completely crash the IDE (usually), but stay safe and commit often!
* This format **does not save instance IDs or tile IDs**. If your game relies on these having exact values, rework your game to not do that, or don't use this format. The ordering of instances, and the ordering of tiles within layers, is preserved, however.
* The format relies on **every asset having a unique name**. You can have a sprite called `player` and an object called `player`, but you can't have two sprites both called `player`. If this isn't the case, saving will fail. Pro tip: use Scripts > Check Resource Names.
* **Timestamps** are also currently not preserved. In practice, all this means is that the "Keep Last Changed" option won't work correctly when importing resources.
* Currently, when you save a new .gm82 file, it will make a load of folders next to it to contain all the resources. **I recommend saving new projects to an empty folder.** This may change in the future.
