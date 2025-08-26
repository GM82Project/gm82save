# gm82save
This is a mod for GameMaker 8.1 with many new features, chief among them being a new .gm82 project format, which plays nicer with git.

* New features:
  * Compatible with version control systems such as Git, SVN, and Mercurial. Never deal with email chains or Dropbox crashes again!
  * Scale and colour individual instances and tiles using [gm82room](https://github.com/GM82Project/gm82room), the new room editor. Instances can also be rotated!
  * Vastly improved load, save, and build times
  * Exported games use more efficient compression
  * When a project is modified by external programs while GameMaker is open, a warning will be shown, allowing you to reload the project or overwite the external changes
  * Many bugs, crashes, and memory leaks from GameMaker 8.1 have been fixed
* Potential pitfalls:
  * This **may still contain bugs**, so keep a backup of your .gm81 if you're converting a project to this. Let me know about any bugs you find.
  * If saving fails partway through, **your files may be inconsistent**. It shouldn't completely crash the IDE (usually), but stay safe and commit often!
  * Saving gm82 projects to a Dropbox folder is currently **not recommended**. I've had at least one report of this somehow crashing Game Maker entirely, and it's not easy to replicate.
  * This format **does not save instance IDs or tile IDs**. If your game relies on these having exact values, rework your game to use them via fields in gm82room, or don't use this format. The ordering of instances, and the ordering of tiles within layers, is preserved, however.
  * The format relies on **every asset having a unique name**. You can't have the same name but in uppercase either. You can have a sprite called `player` and an object called `player`, but you can't have two sprites both called `player`, or two timelines called `player` and `PlAyEr`. Additionally, reserved Windows filenames such as "con" or "nul" also can't be used as resource names. If this isn't the case, saving will fail. Pro tip: click the broom icon next to the Debug button to scan the project for duplicate names.
* Minor quirks:
  * When using the Save As dialog to save a new .gm82 project, it will create a new folder and save into that. Behaviour for saving .gm81 projects is unchanged.
  * I recommend **adding antivirus exceptions** to your GameMaker and project directories. I'm not gonna knock you for being cautious, but antivirus can make saving and loading take quite a lot longer.
  * **Included files** stored outside your project **will be copied into it**, even if "Store in the editable gmk file" is unchecked.
  * **Timestamps** are currently **not preserved**. In practice, all this means is that the "Keep Last Changed" option won't work correctly when importing resources.
