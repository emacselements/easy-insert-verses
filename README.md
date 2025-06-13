# easy-insert-verses

**easy-insert-verses** is an Emacs package that makes it easy to insert Bible verses, chapters, or random selections from the King James Version (KJV) into your buffers. Designed for writers, researchers, and anyone who wants quick access to scripture within Emacs.

## Features

- **Insert Random Verses:** Instantly insert a specified number of random verses from the KJV.
- **Insert Chapters or Ranges:** Extract and insert entire chapters or custom verse ranges.
- **Regex Search:** Search for verses matching a regular expression and insert all matching results.
- **Simple Interactive Commands:** All features are accessible via interactive commands and convenient keybindings.

## Installation

1. **Clone the repository:**
   ```sh
   git clone https://github.com/gnarledgrip/easy-insert-verses.git
   ```
2. **Add to your Emacs `load-path`:**
   ```elisp
   (add-to-list 'load-path "~/easy-insert-verses")
   (require 'easy-insert-verses)
   ```
   Adjust the path if you cloned elsewhere.

3. **Ensure you have a KJV verses file:**  
   By default, the package expects `kjv.txt` in the repo directory.  
   Each line should be a single verse (e.g., `Genesis|1|1|In the beginning...`).

## Usage

### Commands

- <kbd>C-c i</kbd> — `insert-random-verses`:  
  Prompt for a number `N`, then insert `N` random verses.

- <kbd>C-c c</kbd> — `insert-chapters`:  
  Prompt for a regex pattern (e.g., `Genesis|1|` for the first chapter), then insert all matching verses.

- <kbd>C-c v</kbd> — `insert-verses`:  
  Prompt for a start pattern and an end pattern, then insert all verses in that range.

- <kbd>C-c s</kbd> — `search-kjv`:  
  Prompt for a regex pattern, then insert all verses matching the pattern.

### Example

Insert all verses from John chapter 3:
1. Press <kbd>C-c c</kbd>
2. Enter: `John|3|`

Search for all verses containing "faith":
1. Press <kbd>C-c s</kbd>
2. Enter: `faith`

## Customization

- You can edit the path to `kjv.txt` in the source if your file is elsewhere.
- The code can be easily adapted for other verse files or translations with a similar format.

## Watch a Video on It

[https://youtu.be/oZhjm-KSuLE](https://youtu.be/oZhjm-KSuLE)

## License

MIT License

---

**easy-insert-verses** is maintained by [gnarledgrip](https://github.com/gnarledgrip).
