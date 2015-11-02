# plist-buddy

Remote monad wrapper around the plistbuddy shell command for editing plists.

From the manual: The following commands are used to manipulate plist data:

Command | Notes
--------|---------- 
Help    |   Prints this information.
Exit    |  Exits the program. Changes are not saved to the file.
Save    |  Saves the current changes to the file.
Revert  |   Reloads the last saved version of the file.
Clear type |  Clears out all existing entries, and creates root of type type.  See below for a list of types.
Print [entry] |  Prints value of entry.  If an entry is not specified, prints entire file. See below for an explanation of how entry works.
Set entry value | Sets the value at entry to value.
Add entry type [value] | Adds entry with type type and optional value value.  See below for a list of types.
Copy entrySrc entryDst | Copies the entrySrc property to entryDst.
Delete entry | Deletes entry from the plist.
Merge file [entry] | Adds the contents of plist file to entry.
Import entry file  | Creates or sets entry to the contents of file.
