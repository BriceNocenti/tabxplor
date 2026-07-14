# Color Vision Deficiency And OKLCH-Based Palette Design For Crosstables


## Executive summary


Color vision deficiency (CVD) is common, especially among men, and most cases are red–green defects (protan and deutan) that affect how users perceive hue and brightness contrasts in interfaces and data visualizations. Blue–yellow defects (tritan) and complete color blindness (achromatopsia) exist but are much rarer. For UI and statistical graphics, accessible design therefore focuses on avoiding problematic color pairs (especially red/green), ensuring sufficient luminance contrast, and never relying on color as the sole channel for information.[^1][^2][^3][^4][^5][^6][^7]


For diverging palettes that highlight over‑ and under‑representation in crosstables, research and practice converge on a few robust principles: use colorblind‑safe hue pairs such as blue–orange or blue–red instead of red–green; rely on perceptually uniform spaces (such as OKLCH) to control lightness and chroma systematically; respect WCAG’s non‑text contrast requirements (≥ 3∶1 for graphical objects); and test palettes with simulators for protanopia, deuteranopia, and tritanopia. The OKLCH palettes you provided already use a green–blue–violet axis for positive values and a yellow–orange axis for negative values, which is broadly favorable for red–green CVD but can be further optimized by tightening lightness ramps, slightly reducing chroma for extreme colors, and aligning hues more closely with empirically validated colorblind‑safe diverging schemes.[^8][^9][^10][^11][^12][^13][^14]


The remainder of this report synthesizes current knowledge on types of CVD and their confusion axes, codifies widely accepted UI and data‑viz best practices, and then analyzes your light and dark OKLCH palettes with concrete recommendations for improving accessibility for color‑blind users while retaining their perceptual advantages.


***


## Types of color vision deficiency


### Physiological basis and classification


Human photopic color vision relies on three cone types with different spectral sensitivities: L‑cones (long‑wavelength, “red”), M‑cones (medium‑wavelength, “green”), and S‑cones (short‑wavelength, “blue”). Congenital CVD arises when one class of cones is missing or its pigment is spectrally shifted, producing either dichromatic vision (one cone class missing) or anomalous trichromacy (all three cones present but one class has abnormal sensitivity). Acquired CVD, often associated with ocular or neurological disease or medication, tends to be tritan‑like and can differ between eyes, but from a UI and visualization perspective the practical design constraints are similar: avoid relying on a single hue axis and ensure robust luminance contrast.[^3][^15][^16][^17]


Standard clinical and psychophysical classification systems therefore distinguish:


- **Normal trichromats**: all three cone pigments within normal spectral ranges.[^15]
- **Anomalous trichromats**: three cone types present, but one pigment shifted, yielding reduced discrimination along a specific axis (protanomaly, deuteranomaly, tritanomaly).[^18][^3]
- **Dichromats**: only two cone types functional (protanopia, deuteranopia, tritanopia), leading to strong color confusions along characteristic “confusion lines” in color spaces like CIE xyY.[^16]
- **Monochromats (achromatopsia)**: no functional cones, vision effectively grayscale with poor acuity and pronounced photophobia.[^1][^18]


### Red–green deficiencies: protan and deutan


Red–green CVD is by far the most prevalent form, affecting roughly 8% of men and under 1% of women worldwide, with deutan defects more common than protan. These conditions are typically X‑linked, so they disproportionately affect males, and they are particularly disruptive in interfaces that use red vs green as primary semantic colors (for status, validation, or diverging data).[^2][^4][^19][^3]


- **Protanopia (dichromat) and protanomaly (anomalous trichromat)**: L‑cone function is absent or shifted, leading to reduced sensitivity in the long‑wavelength (red) region and a general dimming of reds. Protan observers confuse reds with dark greens, browns, and blacks, and find red–green, red–orange, and red–brown distinctions especially challenging.[^13][^3][^18]
- **Deuteranopia and deuteranomaly**: M‑cone function absent or shifted, producing similar red–green confusion but without the pronounced luminance loss for reds. Deutan observers often cannot reliably distinguish between reds, greens, browns, and some oranges, with pastel tones particularly problematic.[^10][^3][^18]


Psychophysically, both protan and deutan defects are characterized by confusion lines that run through red, orange, green, and brown regions in chromaticity diagrams, meaning colors along these lines can appear indistinguishable even if their physical hues are quite different. In practical terms, any diverging palette that uses red vs green, or mixes both in intermediate steps, risks collapsing into an ambiguous or misleading ramp for a sizable fraction of users.[^19][^6][^20][^16]


### Blue–yellow deficiencies: tritan


Tritan defects are much rarer (often estimated around 0.01% of the population) and can be both congenital and acquired. They involve absent or abnormal S‑cone function, altering perception primarily along the blue–yellow axis rather than red–green.[^4][^17][^3][^18][^1]


People with tritanopia or tritanomaly struggle to distinguish blue from green, yellow from violet, yellow from light gray, and dark blue from black, with orange and pink also tending to converge. The world may appear dominated by pinkish and cyan‑teal tones, and common UI semantics such as blue for “info” and yellow for “warning” can be ambiguous without additional cues.[^21][^7][^18][^1]


Because most scientific and statistical palettes historically focus on red–green accessibility, tritan safety is often overlooked, but modern guidance increasingly recommends designing for all three CVD types by choosing hue combinations that remain distinct under protan, deutan, and tritan simulations (for example, vermillion vs blue vs bluish‑green vs reddish‑purple).[^22][^21]


### Achromatopsia and severe low‑vision conditions


Complete color vision deficiency (rod monochromacy / achromatopsia) is extremely rare, typically affecting a tiny fraction of the population (on the order of 0.003%). In these cases, all cone‑mediated color information is lost, and vision relies on rods, so only luminance differences are perceived.[^3][^18][^1]


Users with achromatopsia, as well as many with severe low vision, benefit primarily from strong lightness contrast, clear shapes and patterns, and textual or symbolic redundancy; whether a palette is “colorblind‑safe” in chromatic terms is almost irrelevant compared to luminance ramps and structural cues. Designing diverging scales in a perceptually uniform space such as OKLCH inherently helps here, because equal steps in L correspond more closely to equal perceived brightness differences, which remain meaningful in grayscale.[^5][^9][^7][^23][^24]


### Prevalence and implications for UI design


Population‑level studies and clinical reviews converge on the conclusion that red–green CVD (protan + deutan) accounts for the vast majority of color blindness: around 8% of men and under 1% of women globally. Blue–yellow (tritan) defects and achromatopsia are orders of magnitude rarer but still relevant when designing critical systems that must remain interpretable for diverse user groups, including those with acquired CVD in older age.[^25][^2][^4][^16][^3]


For UI and data visualization, this implies that the minimum responsible design standard is to:


- Avoid red–green as the primary distinction axis.
- Ensure luminance contrast that meets or exceeds WCAG recommendations.
- Provide non‑color channels (text, icons, patterns) for important semantic distinctions.
- Test palettes against protan, deutan, and tritan simulations rather than relying solely on intuition or nominal hue differences.[^6][^7][^14][^5]


***


## Color confusions and safe hue pairs


### Commonly confused color pairs by type


Empirical psychophysical work and practical design guides identify recurring confusion patterns for CVD types that are directly relevant when constructing palettes.[^26][^16][^10][^13]


For **protan and deutan** observers (red–green axis):


- Red vs green, red vs brown, and red vs orange are highly confusable, especially at similar lightness levels.[^18][^6]
- Green vs brown and green vs gray can be difficult to distinguish when chroma is low.[^10][^26]
- Green vs blue and green vs black are problematic in some contexts, particularly for desaturated or dark tones.[^26][^10]


For **tritan** observers (blue–yellow axis):


- Blue vs green and blue vs black can collapse into similar dark tones.[^21][^1][^18]
- Yellow vs violet and yellow vs light gray are often indistinguishable.[^1][^21]
- Orange vs pink can appear similar, complicating multi‑category palettes that rely on these hues.[^21]


Across CVD types, guides therefore consistently warn against relying on: red–green, green–brown, green–blue, blue–gray, blue–purple, green–gray, green–black, and light green–yellow combinations as the main discriminants in charts and tables. Diverging ramps that pass through greenish midpoints are especially risky because many users may perceive the entire ramp as a small range of similar tones, losing the intended semantic structure.[^20][^10][^26]


### Hue pairs and palettes that are considered safer


Colorblind‑friendly visualization guidance converges on several hue pairs and multi‑color sets that remain distinguishable under protan, deutan, and often tritan simulations.[^14][^6][^13][^10]


Recommended **diverging hue pairs** include:


- Blue vs orange.
- Blue vs red.
- Blue vs brown.
- Purple vs green.
- Cyan vs red.[^6][^20][^10][^26]


Several resources and toolkits, such as the NCEAS guide, ColorBrewer, Paul Tol’s schemes, and colorblind.io, explicitly highlight blue–orange and blue–red diverging palettes as robust options for heatmaps and anomaly plots. ColorBrewer’s catalog labels all sequential palettes and a subset of diverging palettes (for example, BrBG, RdBu, PuOr, PiYG) as colorblind‑safe, while cautioning against Spectral, RdYlGn, and some red–gray ramps because they introduce problematic greens and muddy midpoints.[^11][^12][^13][^20][^6][^26]


For **categorical** palettes, multi‑color sets that have been empirically tested include combinations of vermillion (orange‑red), blue, bluish‑green, reddish‑purple, and black/white, often referred to as the “Wong” or “Tol” palettes. Tritan‑specific guidance notes that orange and sky blue from these sets can be confused by tritan observers, so a tritan‑robust subset (vermillion, blue, reddish‑purple, bluish‑green, black, white) is recommended when blue–yellow safety is required.[^12][^11][^22][^21]


### Role of lightness and chroma in preserving distinguishability


Design resources emphasize that CVD primarily affects hue discrimination, not luminance, so differences in lightness (and to a lesser degree chroma) remain valuable channels even when hue information is degraded. Sequential and diverging palettes that rely on monotonic changes in lightness are therefore easier to interpret than palettes whose main variation is in hue at constant lightness.[^24][^5][^6][^10]


Moreover, extreme chroma at both ends of a ramp can produce large perceived differences for normal trichromats but collapse for CVD observers if both ends lie near the same confusion line; moderate chroma coupled with disciplined lightness differences tends to yield more robust ramps. Perceptually uniform color spaces such as OKLab/OKLCH are specifically designed so that equal numerical changes in lightness and chroma correspond more closely to equal perceptual differences, giving designers a controllable way to construct ramps that remain distinguishable when hue information is partially lost.[^9][^23][^26]


***


## UI and data‑visualization accessibility guidelines


### WCAG contrast requirements for text and non‑text elements


The Web Content Accessibility Guidelines (WCAG) establish minimum contrast ratios to ensure that both text and non‑text UI components are perceivable for users with low vision and CVD.[^27][^28][^29][^8][^5]


Key requirements include:


- **Text contrast (SC 1.4.3 and 1.4.6)**: For normal text, a contrast ratio of at least 4.5∶1 between foreground and background is required at Level AA, with 7∶1 recommended for enhanced readability (AAA).[^5]
- **Non‑text contrast (SC 1.4.11)**: Essential graphical objects and UI components (buttons, form fields, icons, chart elements) must have a contrast ratio of at least 3∶1 against adjacent colors.[^28][^29][^27][^8]


Non‑text contrast explicitly applies to graphical representations required to understand or operate the content, which includes many elements in statistical graphics such as heatmap tiles, axis lines, and selection highlights. Techniques documents recommend testing icons and graphical objects against the darkest adjacent background and adjusting colors, borders, or white space to achieve the 3∶1 ratio.[^30][^8]


### General best practices for color use in UI


Accessibility guides from W3C, MDN, and industry design teams articulate several recurrent principles for using color in interfaces.[^31][^7][^24][^5]


- **Avoid conveying information purely through color**: Always pair color with secondary encodings such as text labels, icons (for example ✓, ✗, ⚠), patterns, or positional cues, so that users with CVD or grayscale vision can still interpret the content.[^7][^24][^6]
- **Limit the number of simultaneous hues**: The more distinct colors used in a single view, the smaller the perceptual differences between them and the harder it becomes for all users, especially those with CVD, to discriminate categories.[^7][^10][^26]
- **Choose hue pairs that avoid common CVD confusions**: Prefer blue–orange, blue–red, purple–green, brown–blue, or similar combinations known to remain distinguishable under red–green CVD, instead of red–green or complex rainbow scales.[^20][^6][^10][^26]
- **Use monotonic lightness ramps for quantitative scales**: For sequential data, vary lightness within a single hue or smooth hue progression; for diverging data, design two branches that each vary lightness away from a neutral midpoint.[^32][^26]


Additional practice‑oriented recommendations emphasize the importance of testing designs with colorblindness simulators (such as Color Oracle, ImageJ’s dichromacy tools, or built‑in OS filters), choosing pre‑tested colorblind‑safe palettes where possible (for example Viridis for continuous scales, ColorBrewer or Tol schemes for discrete and diverging scales), and considering contexts where grayscale reproduction is likely (print, photocopying) so that lightness ramps remain informative even without color.[^14][^6][^7][^20]


### Specific guidance for charts, heatmaps, and crosstables


Data‑visualization‑specific resources discuss how these general principles play out in charts and tabular displays.[^13][^24][^10][^14][^26]


For **heatmaps and diverging crosstables**:


- Avoid classical red–green gradients; they can be nearly unreadable for a large fraction of viewers.[^6][^10][^26]
- Use blue–orange, blue–red, or blue–brown diverging scales, or adopt colorblind‑tested palettes such as Viridis, BrBG, or Tol’s divergent schemes.[^11][^12][^26][^20][^6]
- Ensure that the neutral midpoint (for example, representing zero or average) is perceptually distinct (often a light neutral or muted gray) rather than a saturated color that may confuse users.[^32][^26]


For **categorical overlays or cell states** in tables:


- Reserve a small, consistent set of colors for cell states (for example, “strong over‑representation,” “moderate,” “under‑representation”) and avoid reusing hues for unrelated semantics in the same view.[^10][^13]
- Provide clear legends and, where possible, numeric values inside cells so that color acts as a redundant cue rather than the only information carrier.[^13][^14]


Taken together, these guidelines support your chosen design strategy of using perceptually uniform OKLCH palettes for highlighting deviations in crosstables, while indicating specific directions for refinement: tighter control of lightness ramps, avoidance of greenish midpoints, and alignment of positive and negative hues with empirically validated diverging pairs.


***


## OKLCH and perceptual palette construction


### OKLCH as a perceptual color space


OKLCH is a cylindrical representation of the OKLab color space, with coordinates for lightness (L), chroma (C), and hue angle (h). It was proposed as a more perceptually uniform alternative to older spaces like sRGB, HSL, or even CIELAB, especially for digital design and CSS.[^23][^9]


The key property is that equal numerical differences in OKLCH correspond more closely to equal perceived differences in color than in many other spaces, and that lightness is decoupled from chroma and hue so that gradients can be constructed by varying one dimension at a time. This makes OKLCH particularly suitable for building sequential and diverging palettes where consistent steps in lightness are crucial for readability and where chroma can be tuned to avoid oversaturation or muddy midpoints.[^9][^23]


### General OKLCH strategies for accessible palettes


Guides for OKLCH workflows in UI design suggest several practical strategies:[^23][^9]


- Use **monotonic lightness ramps**: For sequential scales, increase or decrease L monotonically across the range; for diverging scales, design symmetric lightness variation around a neutral midpoint.
- Control **chroma ceilings**: Very high chroma can produce harsh colors that are tiring and may exacerbate confusion for CVD users; moderate C values often balance distinctiveness and comfort.[^9][^23]
- Align hues with **safe axes**: Choose h values that correspond to colorblind‑friendly hue pairs (for example, blue around 240°, orange around 60–80°, vermillion around 20–30°, bluish‑green around 160–170°) rather than problematic red–green axes.[^12][^11][^9]


Because CVD primarily affects hue interpretation, the combination of robust lightness ramps and carefully chosen hue angles allows OKLCH palettes to remain interpretable under protan, deutan, and tritan simulations while still taking advantage of the space’s perceptual uniformity for normal trichromats.[^23][^9][^13]


***


## Analysis of the provided light palette


### Structure of the positive (over‑representation) colors


Your **light text positive** colors are defined (in OKLCH) approximately as:


- 1: L ≈ 0.66, C ≈ 0.13, h ≈ 167 (bluish‑green).
- 2: L ≈ 0.62, C ≈ 0.13, h ≈ 235 (blue).
- 3: L ≈ 0.52, C ≈ 0.17, h ≈ 255 (deep blue).
- 4: L ≈ 0.47, C ≈ 0.30, h ≈ 270 (violet).[^1]


The **light background positive** colors form a ramp:


- 1: L ≈ 0.97, C ≈ 0.03, h ≈ 167.
- 2: L ≈ 0.94, C ≈ 0.0336, h ≈ 235.
- 3: L ≈ 0.91, C ≈ 0.0439, h ≈ 255.
- 4: L ≈ 0.85, C ≈ 0.0733, h ≈ 270.[^1]


This design combines two principles that are generally positive for accessibility: it uses a cool axis (greenish‑cyan to blue to violet) that avoids pure red–green confusions, and it builds a sequence of backgrounds with high lightness (L from 0.97 down to 0.85) and low to moderate chroma so that text or numeric values can stand out against them.[^26][^6][^10]


From a CVD perspective:


- **Protan/deutan**: The bluish‑green (h ≈ 167) and blue/violet tones (h ≈ 235–270) typically remain distinguishable, especially when combined with the lightness differences you have introduced. However, the first color at h ≈ 167 lies near a greenish region that can partially collapse towards gray or brown under red–green CVD, potentially reducing the perceived difference between the lowest positive level and the neutral or slight negative levels.[^6][^10][^26]
- **Tritan**: Blue and violet hues are affected by blue–yellow CVD, but the main confusions arise when comparing blue vs green or blue vs black; your light backgrounds are very bright, so tritan observers are likely to still see a gradient of lightness, though the hue differences between the four steps may be reduced.[^18][^21][^1]


Overall, the positive branch of the light palette is relatively well‑behaved for CVD: it avoids red–green contrasts and uses a controlled lightness ramp. The main improvement opportunity is at the greenish end (h ≈ 167) where a slight shift in hue or tighter chroma control could make the smallest positive deviation more robustly distinguishable from negative or neutral cells for protan/deutan users.


### Structure of the negative (under‑representation) colors


Your **light text negative** color is:


- L ≈ 0.65, C ≈ 0.12, h ≈ 80 (yellow‑orange).[^1]


The **light background negative** ramp consists of:


- 1: L ≈ 0.97, C ≈ 0.0271, h ≈ 80.
- 2: L ≈ 0.94, C ≈ 0.0374, h ≈ 60.
- 3: L ≈ 0.91, C ≈ 0.0488, h ≈ 42.
- 4: L ≈ 0.85, C ≈ 0.082, h ≈ 29.[^1]


This branch moves from a light yellow‑orange (h ≈ 80) towards more saturated orange‑red (h ≈ 29) as lightness decreases, which is consistent with many colorblind‑safe diverging palettes that use orange/red for one side of the scale.[^20][^10][^26][^6]


From a CVD standpoint:


- **Protan/deutan**: Yellow and orange tones remain reasonably distinguishable, especially when paired with blue/violet for the positive branch. However, as chroma increases and L decreases (C up to ≈ 0.082 at L ≈ 0.85), the most saturated negative color may appear very similar to some saturated positive colors for users with strong red–green CVD, particularly if they rely primarily on lightness.[^10][^26][^6]
- **Tritan**: Yellow vs light gray, and orange vs pink, can be confusing, but your negative branch maintains consistent lightness ramps and avoids mixing yellow/orange with close blue backgrounds, which mitigates the most severe tritan confusions.[^18][^21][^1]


Because both positive and negative backgrounds occupy relatively high lightness ranges (L ≥ 0.85), they are likely to satisfy WCAG’s non‑text contrast requirement when combined with appropriately darker text colors, and they remain distinguishable in grayscale. Nonetheless, fine‑tuning of h and C could reduce cross‑branch ambiguities at mid‑levels, particularly for protan/deutan users who might perceive mid‑positive bluish‑green and mid‑negative yellowish tones as more similar than intended when chroma is low.[^8][^30][^5]


### Good practices embodied in the light palette


In relation to documented best practices, your light palette already incorporates several strengths:


- It avoids the classic red vs green diverging axis, instead using blue/violet vs yellow/orange, which is recommended by multiple guides as a colorblind‑friendly alternative.[^26][^6][^10]
- It uses high‑L backgrounds and somewhat darker text colors, supporting strong text/background contrast and compliance with WCAG text and non‑text contrast criteria.[^30][^8][^5]
- It constructs both branches in a perceptually uniform space (OKLCH), allowing controlled manipulation of L and C that aligns with human perception.[^9][^23]


The main enhancement opportunity is to sharpen the perceptual separation between successive levels and between positive and negative branches for CVD users, which can be achieved by slightly increasing lightness differences between steps, moderating chroma for extreme colors, and choosing hue angles that align more directly with empirically validated diverging sets (for example, shifting the greenish positive hue slightly towards cyan or pure blue).[^11][^12][^13]


***


## Analysis of the provided dark palette


### Structure of the positive (over‑representation) colors


Your **dark text positive** colors are approximately:


- 1: L ≈ 0.55, C ≈ 0.10, h ≈ 165 (deep bluish‑green).
- 2: L ≈ 0.60, C ≈ 0.1037, h ≈ 210 (teal‑blue).
- 3: L ≈ 0.62, C ≈ 0.1406, h ≈ 240 (blue).
- 4: L ≈ 0.66, C ≈ 0.1798, h ≈ 265 (blue‑violet).[^1]


The **dark background positive** colors are:


- 1: L ≈ 0.20, C ≈ 0.0418, h ≈ 165.
- 2: L ≈ 0.25, C ≈ 0.0429, h ≈ 210.
- 3: L ≈ 0.30, C ≈ 0.0684, h ≈ 240.
- 4: L ≈ 0.30, C ≈ 0.13, h ≈ 265.[^1]


This dark palette mirrors the hue progression of the light positive branch but with much lower lightness, suitable for dark UIs. The backgrounds cluster in a narrow L range (0.20–0.30) with increasing chroma, while text colors sit around L ≈ 0.55–0.66.[^1]


From a CVD perspective:


- **Protan/deutan**: Again, the greenish hue at h ≈ 165 can be partially problematic, but the strong difference in lightness between text and background ensures readability. The progression from bluish‑green to blue to violet is generally favorable, though dark backgrounds at h ≈ 165 vs h ≈ 210 may be less distinguishable for some users.[^6][^10][^26]
- **Tritan**: Dark blue vs black and blue vs green confusions are more likely in dark contexts; your backgrounds at L ≈ 0.20–0.30 with moderate chroma may appear relatively similar in hue to tritan observers, who then rely mainly on lightness.[^21][^18][^1]


The relatively small L spread among the backgrounds (0.20–0.30) means that, in grayscale or under severe CVD, all four positive background levels may appear quite similar, placing greater reliance on chroma differences that CVD users do not fully perceive. Increasing the lightness spread between dark backgrounds could therefore significantly enhance the perceived progression of positive deviation levels.[^5][^10][^6]


### Structure of the negative (under‑representation) colors


Your **dark text negative** colors are:


- 1: L ≈ 0.60, C ≈ 0.1221, h ≈ 95 (yellow‑green).
- 2: L ≈ 0.64, C ≈ 0.1384, h ≈ 70 (yellow‑orange).
- 3: L ≈ 0.68, C ≈ 0.1792, h ≈ 50 (orange).
- 4: L ≈ 0.70, C ≈ 0.1906, h ≈ 20 (vermillion‑red).[^1]


The **dark background negative** colors are:


- 1: L ≈ 0.20, C ≈ 0.0407, h ≈ 95.
- 2: L ≈ 0.25, C ≈ 0.0537, h ≈ 70.
- 3: L ≈ 0.30, C ≈ 0.0792, h ≈ 50.
- 4: L ≈ 0.35, C ≈ 0.12, h ≈ 20.[^1]


This branch covers yellow‑green to vermillion along a dark background ramp, with modest increases in both L and C. Compared to the light palette, the dark negative branch brings the most saturated vermillion into a somewhat higher lightness (L ≈ 0.35), which is helpful for contrast with very dark surroundings.[^5][^6]


From a CVD viewpoint:


- **Protan/deutan**: Yellow‑green (h ≈ 95) is particularly problematic for red–green CVD, as it lies directly in the confusion region; these users may perceive the first negative level as dull gray or brown, reducing its distinctiveness. Orange and vermillion (h ≈ 50 and 20) are generally more robust, but the interplay with dark blue backgrounds must be checked in simulations.[^3][^18][^10][^26][^6]
- **Tritan**: Yellow vs light gray and orange vs pink confusions exist, but in a dark UI the combination of L and C differences remains workable when paired with blue/violet on the positive branch.[^18][^21][^1]


As with the positive branch, the limited lightness spread on dark backgrounds (L = 0.20–0.35) reduces the perceptible gradation of negative levels for users who primarily perceive luminance, suggesting that a stronger L ramp (for example 0.18, 0.25, 0.32, 0.40) might make the hierarchy of deviations clearer.[^5][^10][^6]


### Good practices embodied in the dark palette


The dark palette mirrors several strengths of the light palette:


- It maintains blue/violet vs yellow/orange/vermillion as the primary diverging axis, avoiding red–green pairings.[^10][^26][^6]
- It provides darker backgrounds for a dark UI and brighter text colors for contrast, aligning with WCAG’s emphasis on minimum 3∶1 contrast for non‑text graphical elements and 4.5∶1 for text.[^8][^30][^5]
- It uses OKLCH coordinates consistently, enabling systematic tuning of lightness and chroma.[^23][^9]


The principal issues relate to the relatively narrow lightness ranges among backgrounds and the use of yellow‑green hues near h ≈ 95 for the weakest negative level, which is particularly vulnerable under red–green CVD. Shifting this hue towards a more neutral or orange direction, and expanding the L ramp, would likely improve accessibility without sacrificing the intended semantic mapping.


***


## Recommendations for improving the palettes for color‑blind users


### General design recommendations grounded in CVD research


Based on the synthesized evidence, several general recommendations apply directly to your crosstable palettes:[^7][^14][^13][^5][^6][^10]


- **Strengthen lightness differentiation in OKLCH**: For both light and dark palettes, ensure that successive levels of positive and negative deviation differ clearly in L; for dark backgrounds, consider larger steps (for example, 0.18, 0.25, 0.32, 0.40) so that grayscale and CVD views still show a clear gradation.[^5][^6][^10]
- **Moderate extreme chroma**: High chroma values (C ≥ 0.18–0.20 in your dark text colors) can be visually demanding and may not yield additional discriminability for CVD users; slightly reducing C while preserving L differences can maintain distinctiveness and comfort.[^9][^23][^26]
- **Align hues with empirically safe axes**: Anchor positive and negative branches to hue angles that correspond to proven colorblind‑safe diverging pairs: for example, positive branch centered around blue/violet (h ≈ 240–270) and negative branch around orange/vermillion (h ≈ 20–60).[^12][^11][^20][^26][^6]


Additionally, since your use case involves cross‑tabular displays with numeric values, ensure that colors are always accompanied by numerical cell contents and, if possible, legends or labels explaining the degree of over‑ or under‑representation; color should act as a redundant channel, not the sole source of information.[^24][^14][^13]


### Specific suggestions for the light palette in OKLCH terms


For the **positive light branch** (L ≈ 0.97–0.85, h ≈ 167–270):


- Consider shifting the greenish hue from h ≈ 167 towards a more cyan/blue hue (for example h ≈ 190–210) to reduce potential greenish confusions for protan/deutan users while retaining a cool feel.[^26][^6][^10]
- Maintain a monotonic decrease in L with each level (for example, refine to L ≈ 0.97, 0.93, 0.89, 0.83) and adjust C so that chroma increases gently without creating overly saturated colors that may dominate.[^23][^9]


For the **negative light branch** (L ≈ 0.97–0.85, h ≈ 80–29):


- Keep the orange/vermillion hues (h ≈ 29–60) as the main negative axis, which is well supported by colorblind‑safe diverging palettes.[^20][^6][^10][^26]
- Consider making the first negative level slightly less yellow‑green and more neutral or orange (for example, changing h ≈ 80 to h ≈ 60 with lower C) to avoid the problematic yellow‑green region for red–green CVD.[^3][^18][^10][^26]


Across both branches, ensure that **text colors** have enough L difference relative to backgrounds to comfortably meet 4.5∶1 contrast in typical sRGB or display environments; OKLCH is perceptual but contrast computations in WCAG still rely on relative luminance in sRGB, so cross‑checking approximate sRGB encodings of your OKLCH choices with a contrast analyzer is advisable.[^30][^8][^5]


### Specific suggestions for the dark palette in OKLCH terms


For the **positive dark branch** (backgrounds L ≈ 0.20–0.30, h ≈ 165–265):


- Increase the lightness span among backgrounds (for example, L ≈ 0.18, 0.24, 0.30, 0.36) so that users who primarily perceive luminance can distinguish levels of deviation even under CVD or grayscale.[^6][^10][^5]
- As with the light palette, shift the most greenish hue (h ≈ 165) towards cyan or blue (h ≈ 190–220) to minimize confusion for protan/deutan observers while keeping a cool positive branch.[^10][^26][^6]


For the **negative dark branch** (backgrounds L ≈ 0.20–0.35, h ≈ 95–20):


- Reduce reliance on yellow‑green at h ≈ 95 for the weakest negative level; consider moving this towards a more neutral dark gray or a less saturated orange with slightly higher L (for example, L ≈ 0.24, C ≈ 0.03, h ≈ 60).[^3][^18][^26][^10]
- Preserve vermillion at h ≈ 20 for the strongest negative levels, as this hue is widely used in colorblind‑safe palettes and remains reasonably distinct from blue/violet for most CVD types.[^22][^11][^12][^6]


For **text colors** in the dark palette, ensure that their L values (currently ≈ 0.55–0.70) yield sufficient contrast against background L values; in dark UIs, many accessibility guides suggest aiming for at least 4.5∶1 contrast even for larger text to account for older users and display variability.[^31][^24][^5]


### Integration with CVD simulations and R workflows


Given that your palettes are used in an R package for crosstables, an evidence‑based workflow would include:


- **Implementing colorblindness simulations** (for protanopia, deuteranopia, tritanopia) using existing R packages or external tools, and testing example crosstables with your OKLCH palettes to visually verify that patterns of over‑ and under‑representation remain legible.[^14][^7][^6]
- **Providing an alternative grayscale or single‑hue sequential palette** that uses lightness only (for example, OKLCH ramp with fixed h and C but varying L), for situations where color must be entirely secondary.[^26][^6][^10]
- **Documenting your OKLCH design rationale** in the package vignette, including references to colorblind‑safe guidelines and contrast requirements, so that users understand the accessibility considerations embedded in the palettes.[^13][^9][^23]


These steps would align your package with contemporary expectations for accessible scientific visualization and give users confidence that the colors chosen are grounded in both perceptual theory (OKLCH) and empirical research on color vision deficiency.


***


## Conclusion


Modern understanding of color vision deficiency emphasizes the dominance of red–green defects (protan, deutan), the specific confusion axes for each CVD type, and the importance of luminance‑based design for both UI and data visualization. Perceptually uniform color spaces such as OKLCH provide a powerful framework for constructing palettes that respect these constraints by enabling controlled variation in lightness, chroma, and hue.[^2][^4][^16][^3][^9][^23]


Your current light and dark palettes already adhere to several best practices, notably avoiding red–green diverging axes and using cool vs warm branches with high‑L backgrounds. However, targeted adjustments to hue angles (away from yellow‑green and towards cyan/blue or orange), expanded lightness ramps for dark backgrounds, and modest chroma moderation for extreme colors would further enhance accessibility for color‑blind users while retaining the palettes’ interpretive clarity for normal trichromats.[^20][^5][^6][^10][^26]


Combined with explicit use of numeric values, legends, and (where relevant) icons or patterns, these palette refinements will help ensure that crosstable highlights of under‑ and over‑representation remain robustly interpretable across the full spectrum of human color vision.

---

## References

1. [Types of Color Vision Deficiency - National Eye Institute - NIH](https://www.nei.nih.gov/eye-health-information/eye-conditions-and-diseases/color-blindness/types-color-vision-deficiency) - Different types of color blindness cause problems seeing different colors. Read about red-green colo...

2. [A Global Perspective of Color Vision Deficiency - PMC - NIH](https://pmc.ncbi.nlm.nih.gov/articles/PMC12385717/) - Color vision deficiency (CVD), commonly referred to as color blindness, affects a significant portio...

3. [Colour deficiency test](https://www.mun.ca/biology/scarr/Colour_deficiency_test.html)

4. [Types of Color Blindness — Protanopia, Deuteranopia, Tritanopia & More | DeficiencyView](https://deficiencyview.com/color-blindness-types) - Guide to all types of color blindness: protanopia, deuteranopia, tritanopia, protanomaly, deuteranom...

5. [Color contrast - Accessibility - MDN Web Docs - Mozilla](https://developer.mozilla.org/en-US/docs/Web/Accessibility/Guides/Understanding_WCAG/Perceivable/Color_contrast) - The color contrast between background and foreground content (that is, usually text) should be great...

6. [[PDF] Colorblind Safe Color Schemes - NCEAS](https://www.nceas.ucsb.edu/sites/default/files/2022-06/Colorblind%20Safe%20Color%20Schemes.pdf)

7. [Guidelines color blind friendly figures | Netherlands Cancer Institute](https://www.nki.nl/about-us/responsible-research/guidelines-color-blind-friendly-figures)

8. [Non-text Contrast | Pearson Higher Education](https://www.pearson.com/accessibility-guidelines/perceivable-principle/non-text-contrast.html) - Pearson Higher Education guidance for WCAG 1.4.11 level AA, Non-text Contrast

9. [OKLCH Color Space: The Developer's Guide to Perceptually ...](https://colorarchive.org/guides/oklch-color-space-guide/) - OKLCH is a perceptually uniform color space designed for digital design and CSS that solves several ...

10. [The best charts for color blind viewers | Blog - Datylon](https://www.datylon.com/blog/data-visualization-for-colorblind-readers) - Around 300 million people in the world are colorblind, 8% of men and 0.5% of women. Discover how you...

11. [Paul Tol's Color Schemes - CRAN - R Project](https://cran.r-project.org/web/packages/khroma/vignettes/tol.html)

12. [Paul Tol's Notes - SRONpersonalPages.nl](https://sronpersonalpages.nl/~pault/) - Clear colour schemes that also work for colour-blind readers.

13. [Colorblind-Friendly Data Visualization](https://colorblind.io/guides/data-visualization) - Create accessible charts, graphs, and maps for colorblind users. Learn about safe color schemes, sha...

14. [What to consider when visualizing data for colorblind readers](https://www.datawrapper.de/blog/colorblindness-part2) - Different ways to ensure red-/green-/blue-blind readers can read your data visualizations.

15. [CLASSIFICATION OF COLOR VISION DEFECTS](https://www.ncbi.nlm.nih.gov/books/NBK217820/) - This section describes how an individual's color vision is characterized on the basis of color-match...

16. [About color vision defects 1,2](https://www.opticaldiagnostics.com/info/color_vision_defects.html)

17. [Color Vision](https://eyewiki.org/Color_Vision) - Eyewiki article about color vision in humans, and clinical significance.

18. [Types of Color Blindness | Protanopia, Deuteranopia, Tritanopia Guide](https://onlinecolorblindtest.com/types.html) - Detailed guide to all types of color blindness including symptoms, prevalence, and inheritance patte...

19. [Color blindness - Wikipedia](https://en.wikipedia.org/wiki/Color_blindness)

20. [Colorblind-safe palettes in SAS - The DO Loop](https://blogs.sas.com/content/iml/2023/02/01/colorblind-safe-palettes-sas.html) - SAS supports the ColorBrewer system of color palettes from the ColorBrewer website (Brewer and Harro...

21. [How to Design for Tritanopia — Color Guide (2026) - RGBlind](https://rgblind.com/blog/how-to-design-for-tritanopia) - A practical guide to designing for blue-yellow color blindness.

22. [Color Blind Friendly Chart Colors & Palettes (2026) - RGBlind](https://rgblind.com/blog/color-blindness-friendly-chart-colors) - Safe color palettes for color blind users. Hex codes, colors to avoid, and chart-specific guides.

23. [Color Theory Foundations & Harmony · UI/UX Atlas](https://uiuxatlas.com/lessons/color/color-theory-foundations-and-harmony/) - Master perceptual color principles, harmony systems, and modern OKLCH workflows to build palettes th...

24. [Colorblind-Friendly Charts: How to Design Accessible Data ...](https://www.cleanchart.app/blog/accessible-colorblind-charts) - Design colorblind-friendly charts and graphs everyone can read. Colorblind-safe color palettes, patt...

25. [Prevalence of Color Blindness...](https://media.market.us/color-blindness-statistics/) - Color Blindness Statistics (2026): Color blindness, or color vision deficiency, is a visual impairme...

26. [Dos and don'ts for a heatmap color scale - BioTuring Team - Medium](https://bioturing.medium.com/dos-and-donts-for-a-heatmap-color-scale-75929663988b) - Heatmaps. We see them all over the scientific journals — the shading matrices that convey meaningful...

27. [Understanding Success Criterion 1.4.11: Non-text Contrast | WAI](https://www.w3.org/WAI/WCAG21/Understanding/non-text-contrast.html)

28. [WCAG 2.1 - SC 1.4.11 Non-Text Contrast](https://www.boia.org/wcag2/cp/1.4.11) - WCAG 2.1 Check Point 1.4.11 - Non-Text Contrast - Color of visual UI components and graphics require...

29. [WCAG Non-text Contrast Explained](https://www.getstark.co/wcag-explained/perceivable/distinguishable/non-text-contrast/) - WCAG 1.4.11, "Non-text Contrast," expands the concept of contrast beyond text. It focuses on ensurin...

30. [G207: Ensuring that a contrast ratio of 3:1 is provided for icons - W3C](https://www.w3.org/WAI/WCAG21/Techniques/general/G207)

31. [Creating Color Contrast Guidelines to Meet WCAG 2.1 and Beyond](https://medium.com/salesforce-ux/creating-color-contrast-guidelines-to-meet-wcag-2-1-and-beyond-99cee77a7657) - At Salesforce, we want to ensure all our experiences meet or exceed current WCAG (Web Content Access...

32. [Sequential, Diverging, and Categorical Color Palettes - CleanChart](https://www.cleanchart.app/blog/color-palette-types-data-visualization) - Learn the difference between sequential, diverging, and categorical color palettes — and exactly whe...

