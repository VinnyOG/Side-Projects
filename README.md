Side-Projects
=============

These are side projects and assignments in miscellaneous languages

As it turns out, setting the stack to 0x8000 does not give us enough room as it is only 512 bytes above our bootloader's rear end (0x8000-0x7c000 = 1KB). This will be fixed very soon and will involve our segmentation registers ds and es
