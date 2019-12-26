;;; indent-lint-test.el --- Test definitions for indent-lint  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/indent-lint.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test definitions for `indent-lint'.


;;; Code:

(require 'buttercup)
(require 'indent-lint)

(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))

;; (provide 'indent-lint-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; indent-lint-test.el ends here
