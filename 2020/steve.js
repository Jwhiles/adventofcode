
const fs = require('fs');

const readInput = () => {
    var data = fs.readFileSync('./Day04.txt', 'utf8');
    const result = data.split('\n');
    return result.slice(0, -1);
};


const input = readInput()

const passportStrings = input.reduce((acc, curLine) => {
    if (curLine.length === 0) {
        return [ ...acc, ''];
    }
    const lastElement = acc.pop();
    return [ ...acc, `${lastElement} ${curLine}`.trim()];
}, ['']);

const passportObjects = passportStrings.map(passportString =>
    passportString
        .split(' ')
        .map(keyValue => keyValue.split(':'))
        .reduce((acc, [key, value]) => ({ ...acc, [key]: value }), {})
);

const validEyeColors = [
    'amb',
    'blu',
    'brn',
    'gry',
    'grn',
    'hzl',
    'oth'
]; 

const isInteger = c => c >= '0' && c <= '9';
const isHexChar = c => isInteger(c) || (c >= 'a' && c <= 'f');

const requiredFields = [
    { key: 'byr', validation: (value) => value.length === 4 && value >= '1920' && value <= '2002' },
    { key: 'iyr', validation: (value) => value.length === 4 && value >= '2010' && value <= '2020' },
    { key: 'eyr', validation: (value) => value.length === 4 && value >= '2020' && value <= '2030' },
    { key: 'hgt', validation: (value) => {
        const length = value.slice(0, -2);
        const unit = value.slice(-2);
        return length.split('').every(isInteger)
            && (
                (unit === 'in' && length >= '59' && length <= '76') ||
                (unit === 'cm' && length >= '150' && length <= '193')
            )
    }},
    { key: 'hcl', validation: (value) => {
        return value.length === 7
            && value[0] === '#' 
            && value.slice(1).split('').every(isHexChar);
        }
    },
    { key: 'ecl', validation: (value) => validEyeColors.includes(value) },
    { key: 'pid', validation: (value) => value.length === 9 && value.split('').every(isInteger) }
];

const validPassportObjects = passportObjects.filter(passportObject =>
    requiredFields.every(requiredField =>
        !!passportObject[requiredField.key] &&
        requiredField.validation(passportObject[requiredField.key])
    )
);
console.log(validPassportObjects)

fs.writeFile('./res.json', JSON.stringify(validPassportObjects))
