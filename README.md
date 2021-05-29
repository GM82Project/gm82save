# gm82save
This is a hack that allows GameMaker 8.1 to save to a custom .gm82 format, which plays nicer with git. There's a few things you should be aware of before working with this.

* Potential pitfalls:
  * This **may still contain bugs**, so keep a backup of your .gm81 if you're converting a project to this. Let me know about any bugs you find.
  * If saving fails partway through, **your files may be inconsistent**. It shouldn't completely crash the IDE (usually), but stay safe and commit often!
  * Saving gm82 projects to a Dropbox folder is currently **not recommended**. I've had at least one report of this somehow crashing Game Maker entirely, and it's not easy to replicate.
  * This format **does not save instance IDs or tile IDs**. If your game relies on these having exact values, rework your game to not do that, or don't use this format. The ordering of instances, and the ordering of tiles within layers, is preserved, however.
  * The format relies on **every asset having a unique name**. You can't have the same name but in uppercase either. You can have a sprite called `player` and an object called `player`, but you can't have two sprites both called `player`, or two timelines called `player` and `PlAyEr`. If this isn't the case, saving will fail. Pro tip: use Scripts > Check Resource Names.
* Minor quirks:
  * When using the Save As dialog to save a new .gm82 project, it will create a new folder and save into that. Behaviour for saving .gm81 projects is unchanged.
  * I recommend **adding antivirus exceptions** to your GameMaker and project directories. I'm not gonna knock you for being cautious, but antivirus can make saving and loading take quite a lot longer.
  * **Timestamps** are currently **not preserved**. In practice, all this means is that the "Keep Last Changed" option won't work correctly when importing resources.
